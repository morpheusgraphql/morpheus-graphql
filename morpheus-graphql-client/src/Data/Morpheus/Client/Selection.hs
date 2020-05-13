{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Selection
  ( operationTypes,
  )
where

--
-- MORPHEUS
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
  ( ReaderT (..),
  )
import Data.Morpheus.Error
  ( deprecatedField,
    globalErrorMessage,
  )
import Data.Morpheus.Internal.Utils
  ( nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ClientType (..),
    ConsD (..),
    DataEnumValue (..),
    DataTypeKind (..),
    FieldDefinition (..),
    GQLErrors,
    Key,
    Name,
    Operation (..),
    RAW,
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TRUE,
    TypeContent (..),
    TypeD (..),
    TypeDefinition (..),
    TypeRef (..),
    UnionTag (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    lookupDeprecated,
    lookupDeprecatedReason,
    removeDuplicates,
    toAny,
    typeFromScalar,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
    Listable (..),
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
    resolveUpdates,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    pack,
  )

compileError :: Text -> GQLErrors
compileError x =
  globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

type Env = (Schema, VariableDefinitions RAW)

newtype Converter a = Converter
  { runConverter ::
      ReaderT
        Env
        Eventless
        a
  }
  deriving (Functor, Applicative, Monad, MonadReader Env)

instance Failure GQLErrors Converter where
  failure = Converter . lift . failure

renderArguments :: VariableDefinitions RAW -> Text -> Maybe TypeD
renderArguments variables argsName
  | null variables = Nothing
  | otherwise = Just rootArgumentsType
  where
    rootArgumentsType :: TypeD
    rootArgumentsType =
      TypeD
        { tName = argsName,
          tNamespace = [],
          tCons = [ConsD {cName = argsName, cFields = map fieldD (toList variables)}],
          tMeta = Nothing
        }
      where
        fieldD :: Variable RAW -> FieldDefinition
        fieldD Variable {variableName, variableType} =
          FieldDefinition
            { fieldName = variableName,
              fieldArgs = NoArguments,
              fieldType = variableType,
              fieldMeta = Nothing
            }

renderOperationArguments :: Operation VALID -> Converter (Maybe TypeD)
renderOperationArguments Operation {operationName} = do
  variables <- asks snd
  pure $ renderArguments variables (getOperationName operationName <> "Args")

renderOperationType :: Operation VALID -> Converter (Maybe TypeD, [ClientType], [Name])
renderOperationType op@Operation {operationName, operationSelection} = do
  datatype <- asks fst >>= getOperationDataType op
  arguments <- renderOperationArguments op
  (outputTypes, enums) <-
    genRecordType
      []
      (getOperationName operationName)
      (toAny datatype)
      operationSelection
  pure (arguments, outputTypes, enums)

operationTypes ::
  Schema ->
  VariableDefinitions RAW ->
  Operation VALID ->
  Eventless (Maybe TypeD, [ClientType])
operationTypes schema vars = flip runReaderT (schema, vars) . runConverter . genOperation

genOperation :: Operation VALID -> Converter (Maybe TypeD, [ClientType])
genOperation operation = do
  (arguments, outputTypes, enums) <- renderOperationType operation
  nonOutputTypes <- renderNonOutputTypes enums
  pure (arguments, outputTypes <> nonOutputTypes)

renderNonOutputTypes :: [Key] -> Converter [ClientType]
renderNonOutputTypes enums = do
  variables <- asks snd
  inputTypeRequests <-
    resolveUpdates [] $
      map (scanInputTypes . typeConName . variableType) (toList variables)
  buildListedTypes (inputTypeRequests <> enums)

-------------------------------------------------------------------------
buildListedTypes :: [Text] -> Converter [ClientType]
buildListedTypes =
  fmap concat . traverse buildInputType . removeDuplicates

-------------------------------------------------------------------------
-- generates selection Object Types
genRecordType ::
  [Name] ->
  Name ->
  TypeDefinition ANY ->
  SelectionSet VALID ->
  Converter ([ClientType], [Name])
genRecordType path tName dataType recordSelSet = do
  (con, subTypes, requests) <- genConsD path tName dataType recordSelSet
  pure
    ( ClientType
        { clientType =
            TypeD
              { tName,
                tNamespace = path,
                tCons = [con],
                tMeta = Nothing
              },
          clientKind = KindObject Nothing
        }
        : subTypes,
      requests
    )

genConsD ::
  [Name] ->
  Name ->
  TypeDefinition ANY ->
  SelectionSet VALID ->
  Converter (ConsD, [ClientType], [Text])
genConsD path cName datatype selSet = do
  (cFields, subTypes, requests) <- unzip3 <$> traverse genField (toList selSet)
  pure (ConsD {cName, cFields}, concat subTypes, concat requests)
  where
    genField ::
      Selection VALID ->
      Converter (FieldDefinition, [ClientType], [Text])
    genField sel =
      do
        (fieldDataType, fieldType) <-
          lookupFieldType
            fieldPath
            datatype
            sel
        (subTypes, requests) <- subTypesBySelection fieldPath fieldDataType sel
        pure
          ( FieldDefinition
              { fieldName,
                fieldType,
                fieldArgs = NoArguments,
                fieldMeta = Nothing
              },
            subTypes,
            requests
          )
      where
        fieldPath = path <> [fieldName]
        -------------------------------
        fieldName = keyOf sel

------------------------------------------
subTypesBySelection ::
  [Name] ->
  TypeDefinition ANY ->
  Selection VALID ->
  Converter ([ClientType], [Text])
subTypesBySelection _ dType Selection {selectionContent = SelectionField} =
  leafType dType
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} =
  genRecordType path (typeFrom [] dType) dType selectionSet
subTypesBySelection path dType Selection {selectionContent = UnionSelection unionSelections} =
  do
    (tCons, subTypes, requests) <-
      unzip3 <$> traverse getUnionType (toList unionSelections)
    pure
      ( ClientType
          { clientType =
              TypeD
                { tNamespace = path,
                  tName = typeFrom [] dType,
                  tCons,
                  tMeta = Nothing
                },
            clientKind = KindUnion
          }
          : concat subTypes,
        concat requests
      )
  where
    getUnionType (UnionTag selectedTyName selectionVariant) = do
      conDatatype <- getType selectedTyName
      genConsD path selectedTyName conDatatype selectionVariant

scanInputTypes :: Key -> [Key] -> Converter [Key]
scanInputTypes name collected
  | name `elem` collected = pure collected
  | otherwise = getType name >>= scanInpType
  where
    scanInpType TypeDefinition {typeContent, typeName} = scanType typeContent
      where
        scanType (DataInputObject fields) =
          resolveUpdates
            (name : collected)
            (map toInputTypeD $ toList fields)
          where
            toInputTypeD :: FieldDefinition -> [Key] -> Converter [Key]
            toInputTypeD FieldDefinition {fieldType = TypeRef {typeConName}} =
              scanInputTypes typeConName
        scanType (DataEnum _) = pure (collected <> [typeName])
        scanType _ = pure collected

buildInputType :: Text -> Converter [ClientType]
buildInputType name = getType name >>= generateTypes
  where
    generateTypes TypeDefinition {typeName, typeContent} = subTypes typeContent
      where
        subTypes :: TypeContent TRUE ANY -> Converter [ClientType]
        subTypes (DataInputObject inputFields) = do
          fields <- traverse toFieldD (toList inputFields)
          pure
            [ mkInputType
                typeName
                KindInputObject
                [ ConsD
                    { cName = typeName,
                      cFields = fields
                    }
                ]
            ]
        subTypes (DataEnum enumTags) =
          pure
            [ mkInputType
                typeName
                KindEnum
                (map enumOption enumTags)
            ]
        subTypes _ = pure []

mkInputType :: Name -> DataTypeKind -> [ConsD] -> ClientType
mkInputType tName clientKind tCons =
  ClientType
    { clientType =
        TypeD
          { tName,
            tNamespace = [],
            tCons,
            tMeta = Nothing
          },
      clientKind
    }

enumOption :: DataEnumValue -> ConsD
enumOption DataEnumValue {enumName} =
  ConsD {cName = enumName, cFields = []}

toFieldD :: FieldDefinition -> Converter FieldDefinition
toFieldD field@FieldDefinition {fieldType} = do
  typeConName <- typeFrom [] <$> getType (typeConName fieldType)
  pure $ field {fieldType = fieldType {typeConName}}

lookupFieldType ::
  [Key] ->
  TypeDefinition ANY ->
  Selection VALID ->
  Converter (TypeDefinition ANY, TypeRef)
lookupFieldType
  path
  TypeDefinition {typeContent = DataObject {objectFields}, typeName}
  Selection
    { selectionName,
      selectionPosition
    } =
    selectBy selError selectionName objectFields >>= processDeprecation
    where
      selError = compileError $ "cant find field \"" <> pack (show objectFields) <> "\""
      processDeprecation FieldDefinition {fieldType = alias@TypeRef {typeConName}, fieldMeta} =
        checkDeprecated >> (trans <$> getType typeConName)
        where
          trans x =
            (x, alias {typeConName = typeFrom path x, typeArgs = Nothing})
          ------------------------------------------------------------------
          checkDeprecated :: Converter ()
          checkDeprecated = case fieldMeta >>= lookupDeprecated of
            Just deprecation -> Converter $ lift $ Success {result = (), warnings, events = []}
              where
                warnings =
                  deprecatedField
                    typeName
                    Ref {refName = selectionName, refPosition = selectionPosition}
                    (lookupDeprecatedReason deprecation)
            Nothing -> pure ()
lookupFieldType _ dt _ =
  failure (compileError $ "Type should be output Object \"" <> pack (show dt))

leafType :: TypeDefinition a -> Converter ([ClientType], [Text])
leafType TypeDefinition {typeName, typeContent} = fromKind typeContent
  where
    fromKind :: TypeContent TRUE a -> Converter ([ClientType], [Text])
    fromKind DataEnum {} = pure ([], [typeName])
    fromKind DataScalar {} = pure ([], [])
    fromKind _ = failure $ compileError "Invalid schema Expected scalar"

getType :: Text -> Converter (TypeDefinition ANY)
getType typename = asks fst >>= selectBy (compileError $ " cant find Type" <> typename) typename

typeFrom :: [Name] -> TypeDefinition a -> Name
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataScalar {} = typeFromScalar typeName
    __typeFrom DataObject {} = nameSpaceType path typeName
    __typeFrom DataUnion {} = nameSpaceType path typeName
    __typeFrom _ = typeName
