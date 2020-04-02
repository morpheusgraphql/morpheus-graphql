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
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Error.Variable   ( incompatibleVariableType )
import           Data.Morpheus.Error.Input      ( Prop(..)
                                                , expectedTypeAFoundB
                                                , unknownField
                                                , undefinedField
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
                                                , Position
                                                , GQLErrors
                                                )
import           Data.Morpheus.Types.Internal.AST.OrderedMap
                                                ( unsafeFromValues )
import           Data.Morpheus.Types.Internal.Operation
                                                ( selectBy
                                                , member
                                                , selectKnown
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..) 
                                                , Validation
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) 
                                                , renderWrapped
                                                )

checkTypeEquality
  :: (Name, [TypeWrapper])
  -> Ref
  -> Variable VALID
  -> Validation ValidValue
checkTypeEquality (tyConName, tyWrappers) Ref { refName, refPosition } Variable { variableValue = ValidVariableValue value, variableType }
  | typeConName variableType == tyConName && not
    (isWeaker (typeWrappers variableType) tyWrappers)
  = pure value
  | otherwise
  = failure $ incompatibleVariableType 
      refName
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
  -> (Message, Position)
  -> [Prop]
  -> [TypeWrapper]
  -> TypeDefinition
  -> (Key, ResolvedValue)
  -> Validation ValidValue
validateInputValue schema ctx props tyWrappers datatype@TypeDefinition { typeContent, typeName } =
  validateWrapped tyWrappers typeContent
 where
  mismatchError :: [TypeWrapper] -> ResolvedValue -> Validation ValidValue
  mismatchError  = typeMismatch ctx props typeName
  -- VALIDATION
  validateWrapped
    :: [TypeWrapper]
    -> TypeContent
    -> (Key, ResolvedValue)
    -> Validation ValidValue
  -- Validate Null. value = null ?
  validateWrapped wrappers _ (_, ResolvedVariable ref variable) =
    checkTypeEquality (typeName, wrappers) ref variable
  validateWrapped wrappers _ (_, Null)
    | isNullableWrapper wrappers = return Null
    | otherwise                  = mismatchError wrappers Null
  -- Validate LIST
  validateWrapped (TypeMaybe : wrappers) _ value =
    validateInputValue schema ctx props wrappers datatype value
  validateWrapped (TypeList : wrappers) _ (key, List list) =
    List <$> mapM validateElement list
   where
    validateElement element =
      validateInputValue schema ctx props wrappers datatype (key, element)
  {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
  {-- VALIDATE OBJECT--}
  validateWrapped [] dt v = validate dt v
   where
    validate
      :: TypeContent -> (Key, ResolvedValue) -> Validation ValidValue
    validate (DataInputObject parentFields) (_, Object fields) = do 
      _ <- traverse requiredFieldsDefined (unFieldsDefinition parentFields)
      Object <$> traverse validateField fields
     where
      requiredFieldsDefined :: FieldDefinition -> Validation ()
      requiredFieldsDefined datafield@FieldDefinition { fieldName }
        | fieldName `member` fields || isFieldNullable datafield = pure ()
        | otherwise = failure (withPrefix ctx $ undefinedField props fieldName)
      validateField
        :: ObjectEntry RESOLVED -> Validation (ObjectEntry VALID)
      validateField ObjectEntry { entryName,  entryValue } = do
          TypeRef { typeConName , typeWrappers } <- fieldType <$> getField
          let currentProp = props <> [Prop entryName typeConName]
          currentTypeName <- lookupInputType typeConName
                                  schema
                                  (mismatch ctx currentProp typeConName typeWrappers entryValue Nothing)
          ObjectEntry entryName 
            <$> validateInputValue 
                  schema
                  ctx
                  currentProp
                  typeWrappers
                  currentTypeName
                  (entryName, entryValue)
       where
        getField = selectKnown (ctx,props,entryName) entryName parentFields
    -- VALIDATE INPUT UNION
    validate (DataInputUnion inputUnion) (_, Object rawFields) =
      case unpackInputUnion inputUnion rawFields of
        Left message -> typeMismatch2 ctx props typeName [] (Object rawFields) (Just message)
        Right (name, Nothing   ) -> return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name)])
        Right (name, Just value) -> do
          currentUnionDatatype <- lookupInputType
            name
            schema
            (mismatch ctx props name [] value Nothing)
          validValue <- validateInputValue schema
                                           ctx
                                           props
                                           [TypeMaybe]
                                           currentUnionDatatype
                                           (name, value)
          return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name), ObjectEntry name validValue])

    {-- VALIDATE ENUM --}
    validate (DataEnum tags) (_, value) =
      validateEnum (mismatch ctx props typeName [] value Nothing) tags value
    {-- VALIDATE SCALAR --}
    validate (DataScalar dataScalar) (_, value) =
      validateScalar dataScalar value (mismatch ctx props typeName [])
    validate _ (_, value) = mismatchError [] value
    {-- 3. THROW ERROR: on invalid values --}
  validateWrapped wrappers _ (_, value) = mismatchError wrappers value


validateScalar
  :: ScalarDefinition
  -> ResolvedValue
  -> (ResolvedValue -> Maybe Message -> GQLErrors)
  -> Validation ValidValue
validateScalar ScalarDefinition { validateValue } value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _            -> return scalarValue
    Left  ""           -> failure (err value Nothing)
    Left  message -> failure $ err value (Just message)
 where
  toScalar :: ResolvedValue -> Validation ValidValue
  toScalar (Scalar x) = pure (Scalar x)
  toScalar scValue    = failure (err scValue Nothing)


withPrefix :: (Message, Position) -> Message -> GQLErrors
withPrefix (prefix,pos) message = errorMessage pos (prefix <> message)

validateEnum :: GQLErrors -> [DataEnumValue] -> ResolvedValue -> Validation ValidValue
validateEnum gqlError enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = failure gqlError
  where tags = map enumName enumValues
validateEnum gqlError _ _ = failure gqlError

typeMismatch2 :: (Message, Position) -> [Prop] -> Name -> [TypeWrapper] -> ResolvedValue -> Maybe Message ->Validation ValidValue
typeMismatch2 (prefix,pos) props typeName wrappers value postfix = failure $ errorMessage pos (prefix <> message)
  where
    message = expectedTypeAFoundB props (renderWrapped typeName wrappers) value postfix

typeMismatch :: (Message, Position) -> [Prop] -> Name -> [TypeWrapper] -> ResolvedValue -> Validation ValidValue
typeMismatch ctx props typeName wrappers value = failure $ withPrefix ctx message
  where
    message = expectedTypeAFoundB props (renderWrapped typeName wrappers) value Nothing

mismatch :: (Message, Position) -> [Prop] -> Name -> [TypeWrapper] -> ResolvedValue -> Maybe Message -> GQLErrors
mismatch (prefix,pos) props typeName wrappers value mess = errorMessage pos (prefix <> message)
  where
    message = expectedTypeAFoundB props (renderWrapped typeName wrappers) value mess