{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInput )
where

import           Data.Foldable                  (traverse_)
import           Data.List                      ( elem )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Error.Variable   ( incompatibleVariableType )
import           Data.Morpheus.Error.Input      ( undefinedField
                                                , typeViolation
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( FieldDefinition(..)
                                                , FieldsDefinition(..)
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
                                                , isFieldNullable
                                                , isNullableWrapper
                                                , ObjectEntry(..)
                                                , RESOLVED
                                                , Position
                                                , GQLError(..)
                                                , GQLErrors
                                                , Path
                                                , Prop(..)
                                                , renderPath
                                                )
import           Data.Morpheus.Types.Internal.AST.OrderedMap
                                                ( unsafeFromValues )
import           Data.Morpheus.Types.Internal.Operation
                                                ( member
                                                , Failure(..) 
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , lookupInputType
                                                , mapError
                                                , selectKnown
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) 
                                                , renderWrapped
                                                )

validateInput
  :: (Message, Position)
  -> [TypeWrapper]
  -> TypeDefinition
  -> ObjectEntry RESOLVED
  -> Validation ValidValue
validateInput ctx = validateInputValue ctx []

violation  :: TypeRef -> ResolvedValue -> Maybe Message -> GQLErrors
violation TypeRef { typeConName , typeWrappers } value _ 
    = globalErrorMessage 
    $ typeViolation (renderWrapped typeConName typeWrappers) value

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
  :: (Message, Position)
  -> Path -- TODO: include Array indexes
  -> [TypeWrapper]
  -> TypeDefinition
  -> ObjectEntry RESOLVED
  -> Validation ValidValue
validateInputValue ctx props tyWrappers TypeDefinition { typeContent = tyCont, typeName } =
  validateWrapped tyWrappers tyCont
 where
  mismatchError :: [TypeWrapper] -> ResolvedValue -> Validation ValidValue
  mismatchError  wrappers x = withContext ctx props $ failure $ violation (TypeRef typeName Nothing wrappers) x Nothing 
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
      traverse_ requiredFieldsDefined (unFieldsDefinition parentFields)
      Object <$> traverse validateField fields
     where
      requiredFieldsDefined :: FieldDefinition -> Validation ()
      requiredFieldsDefined datafield@FieldDefinition { fieldName }
        | fieldName `member` fields || isFieldNullable datafield = pure ()
        | otherwise = withContext ctx props (failure $ globalErrorMessage $ undefinedField fieldName)
      validateField
        :: ObjectEntry RESOLVED -> Validation (ObjectEntry VALID)
      validateField entry@ObjectEntry { entryName,  entryValue } = do
          typeRef@TypeRef { typeConName , typeWrappers } <- withContext ctx props getFieldType
          let currentProp = props <> [Prop entryName typeConName]
          typeDef <- withContext ctx props 
              (lookupInputType
                typeConName                
                (violation typeRef entryValue Nothing)
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
        Left message -> withContext ctx props (failure $ violation (TypeRef typeName Nothing []) (Object rawFields) (Just message))
        Right (name, Nothing   ) -> return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name)])
        Right (name, Just value) -> do
          typeDef <- withContext ctx props $ lookupInputType
            name
            (violation (TypeRef name Nothing []) value Nothing)
          validValue <- validateInputValue 
                              ctx
                              props
                              [TypeMaybe]
                              typeDef
                              (ObjectEntry name value)
          return (Object $ unsafeFromValues [ObjectEntry "__typename" (Enum name), ObjectEntry name validValue])

    {-- VALIDATE ENUM --}
    validate (DataEnum tags) ObjectEntry { entryValue } =
      withContext ctx props $ validateEnum (violation (TypeRef typeName Nothing []) entryValue Nothing) tags entryValue
    {-- VALIDATE SCALAR --}
    validate (DataScalar dataScalar) ObjectEntry { entryValue }  =
      withContext ctx props $ validateScalar dataScalar entryValue (violation (TypeRef typeName Nothing []))
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

validateEnum :: GQLErrors -> [DataEnumValue] -> ResolvedValue -> Validation ValidValue
validateEnum gqlError enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = failure gqlError
  where tags = map enumName enumValues
validateEnum gqlError _ _ = failure gqlError