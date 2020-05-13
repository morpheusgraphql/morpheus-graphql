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
    Position,
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

-- generates argument types for Operation Head
rootArguments :: VariableDefinitions RAW -> Text -> Maybe TypeD
rootArguments variables argsName
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

operationTypes ::
  Schema ->
  VariableDefinitions RAW ->
  Operation VALID ->
  Eventless (Maybe TypeD, [ClientType])
operationTypes schema vars = flip runReaderT (schema, vars) . runConverter . genOperation

genOperation :: Operation VALID -> Converter (Maybe TypeD, [ClientType])
genOperation operation@Operation {operationName, operationSelection} = do
  (lib, variables) <- asks id
  datatype <- getOperationDataType operation lib
  (queryTypes, enums) <-
    genRecordType
      []
      (getOperationName operationName)
      (toAny datatype)
      operationSelection
  inputTypeRequests <-
    resolveUpdates [] $
      map (scanInputTypes . typeConName . variableType) (toList variables)
  inputTypesAndEnums <- buildListedTypes (inputTypeRequests <> enums)
  pure
    ( rootArguments variables (getOperationName operationName <> "Args"),
      queryTypes <> inputTypesAndEnums
    )

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
  (con, subTypes, requests) <- genConsD tName dataType recordSelSet
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
  where
    genConsD ::
      Name ->
      TypeDefinition ANY ->
      SelectionSet VALID ->
      Converter (ConsD, [ClientType], [Text])
    genConsD cName datatype selSet = do
      (cFields, subTypes, requests) <- unzip3 <$> traverse genField (toList selSet)
      pure (ConsD {cName, cFields}, concat subTypes, concat requests)
      where
        genField ::
          Selection VALID ->
          Converter (FieldDefinition, [ClientType], [Text])
        genField
          sel@Selection
            { selectionName,
              selectionPosition
            } =
            do
              (fieldDataType, fieldType) <-
                lookupFieldType
                  fieldPath
                  datatype
                  selectionPosition
                  selectionName
              (subTypes, requests) <- subTypesBySelection fieldDataType sel
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
                TypeDefinition ANY -> Selection VALID -> Converter ([ClientType], [Text])
              subTypesBySelection dType Selection {selectionContent = SelectionField} =
                leafType dType
              --withLeaf buildLeaf dType
              subTypesBySelection dType Selection {selectionContent = SelectionSet selectionSet} =
                genRecordType fieldPath (typeFrom [] dType) dType selectionSet
              ---- UNION
              subTypesBySelection dType Selection {selectionContent = UnionSelection unionSelections} =
                do
                  (tCons, subTypes, requests) <-
                    unzip3 <$> traverse getUnionType (toList unionSelections)
                  pure
                    ( ClientType
                        { clientType =
                            TypeD
                              { tNamespace = fieldPath,
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
                    genConsD selectedTyName conDatatype selectionVariant

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
  Position ->
  Text ->
  Converter (TypeDefinition ANY, TypeRef)
lookupFieldType path TypeDefinition {typeContent = DataObject {objectFields}, typeName} refPosition key =
  selectBy selError key objectFields >>= processDeprecation
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
                  Ref {refName = key, refPosition}
                  (lookupDeprecatedReason deprecation)
          Nothing -> pure ()
lookupFieldType _ dt _ _ =
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
