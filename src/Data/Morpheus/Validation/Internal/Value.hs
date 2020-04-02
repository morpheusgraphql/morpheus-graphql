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
import           Data.Morpheus.Error.Utils      ( errorMessage
                                                , globalErrorMessage
                                                )
import           Data.Morpheus.Error.Variable   ( incompatibleVariableType )
import           Data.Morpheus.Error.Input      ( Prop(..)
                                                , expectedTypeAFoundB
                                                , undefinedField
                                                , typeViolation
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
                                                , GQLError(..)
                                                , GQLErrors
                                                , Path
                                                , renderPath
                                                )
import           Data.Morpheus.Types.Internal.AST.OrderedMap
                                                ( unsafeFromValues )
import           Data.Morpheus.Types.Internal.Operation
                                                ( member
                                                , selectKnown
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..) 
                                                , Validation
                                                , mapError
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) 
                                                , renderWrapped
                                                )


violation  :: Name -> [TypeWrapper] -> ResolvedValue -> Maybe Message -> GQLErrors
violation typeName wrappers value _ 
    = globalErrorMessage 
    $ typeViolation (renderWrapped typeName wrappers) value


withContext :: (Message, Position) -> Path ->  Validation a -> Validation a
withContext (prefix, position) path = mapError addContext
  where 
    addContext GQLError { message, locations} = GQLError (prefix <> renderPath path <>message) (position:locations)

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
  -> ObjectEntry RESOLVED
  -> Validation ValidValue
validateInputValue schema ctx props tyWrappers TypeDefinition { typeContent = tyCont, typeName } =
  validateWrapped tyWrappers tyCont
 where
  mismatchError :: [TypeWrapper] -> ResolvedValue -> Validation ValidValue
  mismatchError  = typeMismatch ctx props typeName
  -- VALIDATION
  validateWrapped
    :: [TypeWrapper]
    -> TypeContent
    -> ObjectEntry RESOLVED
    -> Validation ValidValue
  -- Validate Null. value = null ?
  validateWrapped wrappers _  ObjectEntry { entryValue = ResolvedVariable ref variable} =
    checkTypeEquality (typeName, wrappers) ref variable
  validateWrapped wrappers _ ObjectEntry { entryValue = Null}
    | isNullableWrapper wrappers = return Null
    | otherwise                  = mismatchError wrappers Null
  -- Validate LIST
  validateWrapped (TypeMaybe : wrappers) _ value =
    validateWrapped wrappers tyCont value
  validateWrapped (TypeList : wrappers) _ (ObjectEntry key (List list)) =
    List <$> traverse validateElement list
   where
    validateElement = validateWrapped wrappers tyCont . ObjectEntry key 
  {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
  {-- VALIDATE OBJECT--}
  validateWrapped [] dt v = validate dt v
   where
    validate
      :: TypeContent -> ObjectEntry RESOLVED -> Validation ValidValue
    validate (DataInputObject parentFields) ObjectEntry { entryValue = Object fields} = do 
      _ <- traverse requiredFieldsDefined (unFieldsDefinition parentFields)
      Object <$> traverse validateField fields
     where
      requiredFieldsDefined :: FieldDefinition -> Validation ()
      requiredFieldsDefined datafield@FieldDefinition { fieldName }
        | fieldName `member` fields || isFieldNullable datafield = pure ()
        | otherwise = failure (withPrefix ctx $ undefinedField props fieldName)
      validateField
        :: ObjectEntry RESOLVED -> Validation (ObjectEntry VALID)
      validateField entry@ObjectEntry { entryName,  entryValue } = do
          TypeRef { typeConName , typeWrappers } <- withContext ctx props getFieldType
          let currentProp = props <> [Prop entryName typeConName]
          typeDef <- withContext ctx props 
              (lookupInputType 
                typeConName 
                schema                  
                (violation typeConName typeWrappers entryValue Nothing)
              )
          ObjectEntry entryName 
            <$> validateInputValue 
                  schema
                  ctx
                  currentProp
                  typeWrappers
                  typeDef
                  entry
       where
        getFieldType = fieldType <$> selectKnown entry parentFields
    -- VALIDATE INPUT UNION
    validate (DataInputUnion inputUnion) ObjectEntry { entryValue = Object rawFields} =
      case unpackInputUnion inputUnion rawFields of
        Left message -> typeMismatch2 ctx props typeName [] (Object rawFields) (Just message)
        Right (name, Nothing   ) -> return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name)])
        Right (name, Just value) -> do
          typeDef <- lookupInputType
            name
            schema
            (mismatch ctx props name [] value Nothing)
          validValue <- validateInputValue schema
                                           ctx
                                           props
                                           [TypeMaybe]
                                           typeDef
                                           (ObjectEntry name value)
          return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name), ObjectEntry name validValue])

    {-- VALIDATE ENUM --}
    validate (DataEnum tags) ObjectEntry { entryValue } =
      validateEnum (mismatch ctx props typeName [] entryValue Nothing) tags entryValue
    {-- VALIDATE SCALAR --}
    validate (DataScalar dataScalar) ObjectEntry { entryValue }  =
      validateScalar dataScalar entryValue (mismatch ctx props typeName [])
    validate _ ObjectEntry { entryValue }  = mismatchError [] entryValue
    {-- 3. THROW ERROR: on invalid values --}
  validateWrapped wrappers _ ObjectEntry { entryValue }  = mismatchError wrappers entryValue

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