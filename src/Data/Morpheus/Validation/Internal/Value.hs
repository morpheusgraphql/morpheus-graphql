{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInput )
where

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
                                                , Position
                                                , GQLError(..)
                                                , GQLErrors
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
                                                , lookupInputType
                                                , mapError
                                                , selectKnown
                                                , selectWithDefaultValue
                                                , askScopePosition
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

violation  :: Position -> TypeRef -> ResolvedValue -> Maybe Message -> GQLErrors
violation pos TypeRef { typeConName , typeWrappers } value _ 
    = errorMessage pos 
    $ typeViolation (renderWrapped typeConName typeWrappers) value

castFailure :: TypeRef -> ResolvedValue -> Maybe Message -> Validator a
castFailure ref value message = do
  pos <- askScopePosition
  failure $ violation pos ref value message

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
  validateWrapped tyWrappers tyCont
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
      validateField entry@ObjectEntry { entryName,  entryValue } = do
          typeRef@TypeRef { typeConName , typeWrappers } <- withContext ctx props getFieldType
          let currentProp = props <> [Prop entryName typeConName]
          pos <- askScopePosition 
          typeDef <- withContext ctx props 
              (lookupInputType
                typeConName                
                (violation pos typeRef entryValue Nothing)
              )
          ObjectEntry entryName 
            <$> validateInputValue
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
        Left message -> withContext ctx props $ castFailure (TypeRef typeName Nothing []) (Object rawFields) (Just message)
        Right (name, Nothing   ) -> return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name)])
        Right (name, Just value) -> do
          pos <- askScopePosition
          typeDef <- withContext ctx props $ lookupInputType
            name
            (violation pos (TypeRef name Nothing []) value Nothing)
          validValue <- validateInputValue 
                              ctx
                              props
                              [TypeMaybe]
                              typeDef
                              (ObjectEntry name value)
          return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name), ObjectEntry name validValue])

    {-- VALIDATE ENUM --}
    validate (DataEnum tags) ObjectEntry { entryValue } = do
      pos <- askScopePosition
      withContext ctx props $ validateEnum (violation pos (TypeRef typeName Nothing []) entryValue Nothing) tags entryValue
    {-- VALIDATE SCALAR --}
    validate (DataScalar dataScalar) ObjectEntry { entryValue }  = do
      pos <- askScopePosition
      withContext ctx props $ validateScalar dataScalar entryValue (violation pos (TypeRef typeName Nothing []))
    validate _ ObjectEntry { entryValue }  = mismatchError [] entryValue
    {-- 3. THROW ERROR: on invalid values --}
  validateWrapped wrappers _ ObjectEntry { entryValue }  = mismatchError wrappers entryValue

validateScalar
  :: ScalarDefinition
  -> ResolvedValue
  -> (ResolvedValue -> Maybe Message -> GQLErrors)
  -> Validator ValidValue
validateScalar ScalarDefinition { validateValue } value err = do
  scalarValue <- toScalar value
  case validateValue scalarValue of
    Right _            -> return scalarValue
    Left  ""           -> failure (err value Nothing)
    Left  message -> failure $ err value (Just message)
 where
  toScalar :: ResolvedValue -> Validator ValidValue
  toScalar (Scalar x) = pure (Scalar x)
  toScalar scValue    = failure (err scValue Nothing)

validateEnum :: GQLErrors -> [DataEnumValue] -> ResolvedValue -> Validator ValidValue
validateEnum gqlError enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = failure gqlError
  where tags = map enumName enumValues
validateEnum gqlError _ _ = failure gqlError