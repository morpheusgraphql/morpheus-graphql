{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInput )
where

import           Data.Maybe                     (maybe)
import           Data.Foldable                  (traverse_)
import           Data.List                      ( elem )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Error.Variable   ( incompatibleVariableType )
import           Data.Morpheus.Error.Input      ( typeViolation )
import           Data.Morpheus.Types.Internal.AST
                                                ( FieldDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , ScalarDefinition(..)
                                                , TypeRef(..)
                                                , TypeWrapper(..)
                                                , DataEnumValue(..)
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
                                                , isNullableWrapper
                                                , ObjectEntry(..)
                                                , RESOLVED
                                                , GQLError(..)
                                                , Path
                                                , Prop(..)
                                                , renderPath
                                                , InputFieldsDefinition(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.OrderedMap
                                                ( unsafeFromValues )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Failure(..) )
import           Data.Morpheus.Types.Internal.Validator
                                                ( Validator
                                                , askInputFieldType
                                                , askInputMember
                                                , mapError
                                                , selectKnown
                                                , selectWithDefaultValue
                                                , askScopePosition
                                                , withScopeType
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) 
                                                , renderWrapped
                                                )

validateInput
  :: Message
  -> [TypeWrapper]
  -> TypeDefinition
  -> ObjectEntry RESOLVED
  -> Validator ValidValue
validateInput ctx = validateInputValue ctx []

castFailure :: TypeRef -> ResolvedValue -> Maybe Message -> Validator a
castFailure TypeRef { typeConName , typeWrappers } value message = do
  pos <- askScopePosition
  failure 
    $  errorMessage pos 
    $ typeViolation (renderWrapped typeConName typeWrappers) value <> maybe "" (" " <>) message

withContext :: Message -> Path ->  Validator a -> Validator a
withContext prefix path = mapError addContext
  where 
    addContext GQLError { message, locations} = GQLError (prefix <> renderPath path <> message) locations

checkTypeEquality
  :: (Name, [TypeWrapper])
  -> Ref
  -> Variable VALID
  -> Validator ValidValue
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
  :: Message
  -> Path -- TODO: include Array indexes
  -> [TypeWrapper]
  -> TypeDefinition
  -> ObjectEntry RESOLVED
  -> Validator ValidValue
validateInputValue ctx props tyWrappers TypeDefinition { typeContent = tyCont, typeName } =
  withScopeType typeName 
  . validateWrapped tyWrappers tyCont
 where
  mismatchError :: [TypeWrapper] -> ResolvedValue -> Validator ValidValue
  mismatchError  wrappers x = withContext ctx props $ castFailure (TypeRef typeName Nothing wrappers) x Nothing 
  -- VALIDATION
  validateWrapped
    :: [TypeWrapper]
    -> TypeContent
    -> ObjectEntry RESOLVED
    -> Validator ValidValue
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
      :: TypeContent -> ObjectEntry RESOLVED -> Validator ValidValue
    validate (DataInputObject parentFields) ObjectEntry { entryValue = Object fields} = do 
      traverse_ requiredFieldsDefined (unInputFieldsDefinition parentFields)
      Object <$> traverse validateField fields
     where
      requiredFieldsDefined :: FieldDefinition -> Validator (ObjectEntry RESOLVED)
      requiredFieldsDefined fieldDef@FieldDefinition { fieldName}
        = withContext ctx props $ selectWithDefaultValue (ObjectEntry fieldName Null) fieldDef fields 
      validateField
        :: ObjectEntry RESOLVED -> Validator (ObjectEntry VALID)
      validateField entry@ObjectEntry { entryName } = do
          inputField@FieldDefinition{ fieldType = TypeRef { typeConName , typeWrappers }} <- withContext ctx props getField
          inputTypeDef <- askInputFieldType inputField 
          let currentProp = props <> [Prop entryName typeConName]
          ObjectEntry entryName 
            <$> validateInputValue
                  ctx
                  currentProp
                  typeWrappers
                  inputTypeDef
                  entry
       where
        getField = selectKnown entry parentFields
    -- VALIDATE INPUT UNION
    validate (DataInputUnion inputUnion) ObjectEntry { entryValue = Object rawFields} =
      case unpackInputUnion inputUnion rawFields of
        Left message -> withContext ctx props $ castFailure (TypeRef typeName Nothing []) (Object rawFields) (Just message)
        Right (name, Nothing   ) -> return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name)])
        Right (name, Just value) -> do
          inputDef <- askInputMember name
          validValue <- validateInputValue 
                              ctx
                              props
                              [TypeMaybe]
                              inputDef
                              (ObjectEntry name value)
          return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name), ObjectEntry name validValue])

    {-- VALIDATE ENUM --}
    validate (DataEnum tags) ObjectEntry { entryValue } =
      withContext ctx props $ validateEnum (castFailure (TypeRef typeName Nothing [])) tags entryValue
    {-- VALIDATE SCALAR --}
    validate (DataScalar dataScalar) ObjectEntry { entryValue }  = 
      withContext ctx props $ validateScalar dataScalar entryValue (castFailure (TypeRef typeName Nothing []))
    validate _ ObjectEntry { entryValue }  = mismatchError [] entryValue
    {-- 3. THROW ERROR: on invalid values --}
  validateWrapped wrappers _ ObjectEntry { entryValue }  = mismatchError wrappers entryValue

validateScalar
  :: ScalarDefinition
  -> ResolvedValue
  -> (ResolvedValue -> Maybe Message -> Validator ValidValue)
  -> Validator ValidValue
validateScalar ScalarDefinition { validateValue } value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _            -> return scalarValue
    Left  ""           -> err value Nothing
    Left  message -> err value (Just message)
 where
  toScalar :: ResolvedValue -> Validator ValidValue
  toScalar (Scalar x) = pure (Scalar x)
  toScalar scValue    = err scValue Nothing

validateEnum 
  :: (ResolvedValue -> Maybe Message -> Validator ValidValue) 
  -> [DataEnumValue] 
  -> ResolvedValue 
  -> Validator ValidValue
validateEnum err enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = err (Enum enumValue) Nothing
  where tags = map enumName enumValues
validateEnum err _ value = err value Nothing