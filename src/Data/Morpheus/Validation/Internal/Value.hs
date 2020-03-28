{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInputValue
  , validateEnum
  )
where

import           Data.List                      ( elem )

-- MORPHEUS
import           Data.Morpheus.Error.Variable   ( incompatibleVariableType )
import           Data.Morpheus.Error.Input      ( InputError(..)
                                                , InputValidation
                                                , Prop(..)
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , Schema(..)
                                                , ScalarDefinition(..)
                                                , Key
                                                , TypeRef(..)
                                                , TypeWrapper(..)
                                                , DataEnumValue(..)
                                                , lookupInputType
                                                , Value(..)
                                                , ValidValue
                                                , Variable(..)
                                                , Ref(..)
                                                , Message
                                                , Name
                                                , ResolvedValue
                                                , VALID
                                                , VariableContent(..)
                                                , TypeRef(..)
                                                , isWeaker
                                                , unpackInputUnion
                                                , isFieldNullable
                                                , isNullableWrapper
                                                , ObjectEntry(..)
                                                , RESOLVED
                                                )
import           Data.Morpheus.Types.Internal.AST.OrderedMap
                                                ( unsafeFromValues )
import           Data.Morpheus.Types.Internal.Operation
                                                ( selectBy
                                                , member
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..) )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) 
                                                , renderWrapped
                                                )

checkTypeEquality
  :: (Name, [TypeWrapper])
  -> Ref
  -> Variable VALID
  -> InputValidation ValidValue
checkTypeEquality (tyConName, tyWrappers) Ref { refName, refPosition } Variable { variableValue = ValidVariableValue value, variableType }
  | typeConName variableType == tyConName && not
    (isWeaker (typeWrappers variableType) tyWrappers)
  = pure value
  | otherwise
  = failure $ GlobalInputError $ incompatibleVariableType refName
                                                          varSignature
                                                          fieldSignature
                                                          refPosition
 where
  varSignature   = render variableType
  fieldSignature = render TypeRef { typeConName  = tyConName
                                  , typeWrappers = tyWrappers
                                  , typeArgs     = Nothing
                                  }

-- Validate Variable Argument or all Possible input Values
validateInputValue
  :: Schema
  -> [Prop]
  -> [TypeWrapper]
  -> TypeDefinition
  -> (Key, ResolvedValue)
  -> InputValidation ValidValue
validateInputValue lib props rw datatype@TypeDefinition { typeContent, typeName } =
  validateWrapped rw typeContent
 where
  throwError :: [TypeWrapper] -> ResolvedValue -> InputValidation ValidValue
  throwError wrappers value =
    Left $ UnexpectedType props (renderWrapped typeName wrappers) value Nothing
  -- VALIDATION
  validateWrapped
    :: [TypeWrapper]
    -> TypeContent
    -> (Key, ResolvedValue)
    -> InputValidation ValidValue
  -- Validate Null. value = null ?
  validateWrapped wrappers _ (_, ResolvedVariable ref variable) =
    checkTypeEquality (typeName, wrappers) ref variable
  validateWrapped wrappers _ (_, Null)
    | isNullableWrapper wrappers = return Null
    | otherwise                  = throwError wrappers Null
  -- Validate LIST
  validateWrapped (TypeMaybe : wrappers) _ value =
    validateInputValue lib props wrappers datatype value
  validateWrapped (TypeList : wrappers) _ (key, List list) =
    List <$> mapM validateElement list
   where
    validateElement element =
      validateInputValue lib props wrappers datatype (key, element)
  {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
  {-- VALIDATE OBJECT--}
  validateWrapped [] dt v = validate dt v
   where
    validate
      :: TypeContent -> (Key, ResolvedValue) -> InputValidation ValidValue
    validate (DataInputObject parentFields) (_, Object fields) = do 
      _ <- traverse requiredFieldsDefined (unFieldsDefinition parentFields)
      Object <$> traverse validateField fields
     where
      requiredFieldsDefined datafield@FieldDefinition { fieldName }
        | fieldName `member` fields || isFieldNullable datafield = pure ()
        | otherwise = failure (UndefinedField props fieldName)
      validateField
        :: ObjectEntry RESOLVED -> InputValidation (ObjectEntry VALID)
      validateField ObjectEntry { entryName,  entryValue } = do
        (type', currentProp') <- validationData entryValue
        wrappers'             <- typeWrappers . fieldType <$> getField
        ObjectEntry entryName 
          <$> validateInputValue 
                lib 
                currentProp'
                wrappers'
                type'
                (entryName, entryValue)
       where
        validationData :: ResolvedValue -> InputValidation (TypeDefinition, [Prop])
        validationData x = do
          fieldTypeName' <- typeConName . fieldType <$> getField
          let currentProp = props ++ [Prop entryName fieldTypeName']
          type' <- lookupInputType fieldTypeName'
                                   lib
                                   (typeMismatch x fieldTypeName' currentProp)
          return (type', currentProp)
        getField = selectBy (UnknownField props entryName) entryName parentFields
    -- VALIDATE INPUT UNION
    validate (DataInputUnion inputUnion) (_, Object rawFields) =
      case unpackInputUnion inputUnion rawFields of
        Left message -> failure
          $ UnexpectedType props typeName (Object rawFields) (Just message)
        Right (name, Nothing   ) -> return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name)])
        Right (name, Just value) -> do
          currentUnionDatatype <- lookupInputType
            name
            lib
            (typeMismatch value name props)
          validValue <- validateInputValue lib
                                           props
                                           [TypeMaybe]
                                           currentUnionDatatype
                                           (name, value)
          return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name), ObjectEntry name validValue])

    {-- VALIDATE ENUM --}
    validate (DataEnum tags) (_, value) =
      validateEnum (UnexpectedType props typeName value Nothing) tags value
    {-- VALIDATE SCALAR --}
    validate (DataScalar dataScalar) (_, value) =
      validateScalar dataScalar value (UnexpectedType props typeName)
    validate _ (_, value) = throwError [] value
    {-- 3. THROW ERROR: on invalid values --}
  validateWrapped wrappers _ (_, value) = throwError wrappers value


validateScalar
  :: ScalarDefinition
  -> ResolvedValue
  -> (ResolvedValue -> Maybe Message -> InputError)
  -> InputValidation ValidValue
validateScalar ScalarDefinition { validateValue } value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _            -> return scalarValue
    Left  ""           -> failure (err value Nothing)
    Left  errorMessage -> failure $ err value (Just errorMessage)
 where
  toScalar :: ResolvedValue -> InputValidation ValidValue
  toScalar (Scalar x) = pure (Scalar x)
  toScalar scValue    = Left (err scValue Nothing)

validateEnum
  :: error -> [DataEnumValue] -> ResolvedValue -> Either error ValidValue
validateEnum gqlError enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = Left gqlError
  where tags = map enumName enumValues
validateEnum gqlError _ _ = Left gqlError

typeMismatch :: ResolvedValue -> Key -> [Prop] -> InputError
typeMismatch jsType expected' path' =
  UnexpectedType path' expected' jsType Nothing
