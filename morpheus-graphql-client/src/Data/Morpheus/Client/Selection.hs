{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Error.Warning
  ( deprecatedField,
  )
import Data.Morpheus.Internal.Utils
  ( nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
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
    typeFromScalar,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Listable (..),
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Failure (..),
    LibUpdater,
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

operationTypes ::
  Schema ->
  VariableDefinitions RAW ->
  Operation VALID ->
  Eventless (Maybe TypeD, [ClientType])
operationTypes lib variables = genOperation
  where
    genOperation operation@Operation {operationName, operationSelection} = do
      datatype <- getOperationDataType operation lib
      (queryTypes, enums) <-
        genRecordType
          []
          (getOperationName operationName)
          datatype
          operationSelection
      inputTypeRequests <-
        resolveUpdates [] $
          map (scanInputTypes lib . typeConName . variableType) (toList variables)
      inputTypesAndEnums <- buildListedTypes (inputTypeRequests <> enums)
      pure
        ( rootArguments (getOperationName operationName <> "Args"),
          queryTypes <> inputTypesAndEnums
        )
    -------------------------------------------------------------------------
    buildListedTypes =
      fmap concat . traverse (buildInputType lib) . removeDuplicates
    -------------------------------------------------------------------------
    -- generates argument types for Operation Head
    rootArguments :: Text -> Maybe TypeD
    rootArguments argsName
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
    ---------------------------------------------------------
    -- generates selection Object Types
    genRecordType ::
      [Name] ->
      Name ->
      TypeDefinition ->
      SelectionSet VALID ->
      Eventless ([ClientType], [Name])
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
          TypeDefinition ->
          SelectionSet VALID ->
          Eventless (ConsD, [ClientType], [Text])
        genConsD cName datatype selSet = do
          (cFields, subTypes, requests) <- unzip3 <$> traverse genField (toList selSet)
          pure (ConsD {cName, cFields}, concat subTypes, concat requests)
          where
            genField ::
              Selection VALID ->
              Eventless (FieldDefinition, [ClientType], [Text])
            genField
              sel@Selection
                { selectionName,
                  selectionPosition
                } =
                do
                  (fieldDataType, fieldType) <-
                    lookupFieldType
                      lib
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
                    TypeDefinition -> Selection VALID -> Eventless ([ClientType], [Text])
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
                        conDatatype <- getType lib selectedTyName
                        genConsD selectedTyName conDatatype selectionVariant

scanInputTypes :: Schema -> Key -> LibUpdater [Key]
scanInputTypes lib name collected
  | name `elem` collected = pure collected
  | otherwise = getType lib name >>= scanInpType
  where
    scanInpType TypeDefinition {typeContent, typeName} = scanType typeContent
      where
        scanType (DataInputObject fields) =
          resolveUpdates
            (name : collected)
            (map toInputTypeD $ toList fields)
          where
            toInputTypeD :: FieldDefinition -> LibUpdater [Key]
            toInputTypeD FieldDefinition {fieldType = TypeRef {typeConName}} =
              scanInputTypes lib typeConName
        scanType (DataEnum _) = pure (collected <> [typeName])
        scanType _ = pure collected

buildInputType :: Schema -> Text -> Eventless [ClientType]
buildInputType lib name = getType lib name >>= generateTypes
  where
    generateTypes TypeDefinition {typeName, typeContent} = subTypes typeContent
      where
        subTypes (DataInputObject inputFields) = do
          fields <- traverse toFieldD (toList inputFields)
          pure
            [ ClientType
                { clientType =
                    TypeD
                      { tName = typeName,
                        tNamespace = [],
                        tCons =
                          [ ConsD
                              { cName = typeName,
                                cFields = fields
                              }
                          ],
                        tMeta = Nothing
                      },
                  clientKind = KindInputObject
                }
            ]
          where
            toFieldD :: FieldDefinition -> Eventless FieldDefinition
            toFieldD field@FieldDefinition {fieldType} = do
              typeConName <- typeFrom [] <$> getType lib (typeConName fieldType)
              pure $ field {fieldType = fieldType {typeConName}}
        subTypes (DataEnum enumTags) =
          pure
            [ ClientType
                { clientType =
                    TypeD
                      { tName = typeName,
                        tNamespace = [],
                        tCons = map enumOption enumTags,
                        tMeta = Nothing
                      },
                  clientKind = KindEnum
                }
            ]
          where
            enumOption DataEnumValue {enumName} =
              ConsD {cName = enumName, cFields = []}
        subTypes _ = pure []

lookupFieldType ::
  Schema ->
  [Key] ->
  TypeDefinition ->
  Position ->
  Text ->
  Eventless (TypeDefinition, TypeRef)
lookupFieldType lib path TypeDefinition {typeContent = DataObject {objectFields}, typeName} refPosition key =
  selectBy selError key objectFields >>= processDeprecation
  where
    selError = compileError $ "cant find field \"" <> pack (show objectFields) <> "\""
    processDeprecation FieldDefinition {fieldType = alias@TypeRef {typeConName}, fieldMeta} =
      checkDeprecated >> (trans <$> getType lib typeConName)
      where
        trans x =
          (x, alias {typeConName = typeFrom path x, typeArgs = Nothing})
        ------------------------------------------------------------------
        checkDeprecated :: Eventless ()
        checkDeprecated = case fieldMeta >>= lookupDeprecated of
          Just deprecation -> Success {result = (), warnings, events = []}
            where
              warnings =
                deprecatedField
                  typeName
                  Ref {refName = key, refPosition}
                  (lookupDeprecatedReason deprecation)
          Nothing -> pure ()
lookupFieldType _ _ dt _ _ =
  failure (compileError $ "Type should be output Object \"" <> pack (show dt))

leafType :: TypeDefinition -> Eventless ([ClientType], [Text])
leafType TypeDefinition {typeName, typeContent} = fromKind typeContent
  where
    fromKind :: TypeContent -> Eventless ([ClientType], [Text])
    fromKind DataEnum {} = pure ([], [typeName])
    fromKind DataScalar {} = pure ([], [])
    fromKind _ = failure $ compileError "Invalid schema Expected scalar"

getType :: Schema -> Text -> Eventless TypeDefinition
getType lib typename = selectBy (compileError typename) typename lib

typeFrom :: [Name] -> TypeDefinition -> Name
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataScalar {} = typeFromScalar typeName
    __typeFrom DataObject {} = nameSpaceType path typeName
    __typeFrom DataUnion {} = nameSpaceType path typeName
    __typeFrom _ = typeName
