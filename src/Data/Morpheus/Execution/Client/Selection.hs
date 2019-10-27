{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Client.Selection
  ( operationTypes
  ) where

import           Data.Semigroup                                ((<>))
import           Data.Text                                     (Text, pack, unpack)

--
-- MORPHEUS
import           Data.Morpheus.Error.Utils                     (globalErrorMessage)
import           Data.Morpheus.Execution.Internal.GraphScanner (LibUpdater, resolveUpdates)
import           Data.Morpheus.Execution.Internal.Utils        (nameSpaceType)
import           Data.Morpheus.Types.Internal.AST.Operation    (DefaultValue, Operation (..), ValidOperation,
                                                                Variable (..), VariableDefinitions, getOperationName)
import           Data.Morpheus.Types.Internal.AST.Selection    (Selection (..), SelectionRec (..), SelectionSet,
                                                                ValidSelection)
import           Data.Morpheus.Types.Internal.Data             (DataField (..), DataFullType (..), DataLeaf (..),
                                                                DataTyCon (..), DataTypeKind (..), DataTypeLib (..),
                                                                Key, TypeAlias (..), allDataTypes)
import           Data.Morpheus.Types.Internal.DataD            (ConsD (..), GQLTypeD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Validation       (GQLErrors, Validation)
import           Data.Morpheus.Validation.Internal.Utils       (lookupType)
import           Data.Set                                      (fromList, toList)

removeDuplicates :: [Text] -> [Text]
removeDuplicates = toList . fromList

compileError :: Text -> GQLErrors
compileError x = globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

operationTypes :: DataTypeLib -> VariableDefinitions -> ValidOperation -> Validation (Maybe TypeD, [GQLTypeD])
operationTypes lib variables = genOperation
  where
    genOperation Operation {operationName, operationSelection} = do
      (queryTypes, enums) <- genRecordType [] (getOperationName operationName) queryDataType operationSelection
      inputTypeRequests <- resolveUpdates [] $ map (scanInputTypes lib . variableType . snd) variables
      inputTypesAndEnums <- buildListedTypes (inputTypeRequests <> enums)
      pure (rootArguments (getOperationName operationName <> "Args"), queryTypes <> inputTypesAndEnums)
      where
        queryDataType = OutputObject $ snd $ query lib
    -------------------------------------------------------------------------
    buildListedTypes = fmap concat . traverse (buildInputType lib) . removeDuplicates
    -------------------------------------------------------------------------
    -- generates argument types for Operation Head
    rootArguments :: Text -> Maybe TypeD
    rootArguments argsName
      | null variables = Nothing
      | otherwise = Just rootArgumentsType
        ------------------------------------------
      where
        rootArgumentsType :: TypeD
        rootArgumentsType =
          TypeD
            { tName = unpack argsName
            , tNamespace = []
            , tCons = [ConsD {cName = unpack argsName, cFields = map fieldD variables}]
            }
          where
            fieldD :: (Text, Variable DefaultValue) -> DataField
            fieldD (key, Variable {variableType, variableTypeWrappers}) =
              DataField
                { fieldName = key
                , fieldArgs = []
                , fieldArgsType = Nothing
                , fieldType =
                    TypeAlias {aliasWrappers = variableTypeWrappers, aliasTyCon = variableType, aliasArgs = Nothing}
                , fieldHidden = False
                }
    ---------------------------------------------------------
    -- generates selection Object Types
    genRecordType :: [Key] -> Key -> DataFullType -> SelectionSet -> Validation ([GQLTypeD], [Text])
    genRecordType path name dataType recordSelSet = do
      (con, subTypes, requests) <- genConsD (unpack name) dataType recordSelSet
      pure
        ( GQLTypeD
            { typeD = TypeD {tName, tNamespace = map unpack path, tCons = [con]}
            , typeKindD = KindObject Nothing
            , typeArgD = []
            } :
          subTypes
        , requests)
      where
        tName = unpack name
        genConsD :: String -> DataFullType -> SelectionSet -> Validation (ConsD, [GQLTypeD], [Text])
        genConsD cName datatype selSet = do
          cFields <- traverse genField selSet
          (subTypes, requests) <- newFieldTypes datatype selSet
          pure (ConsD {cName, cFields}, concat subTypes, concat requests)
          ---------------------------------------------------------------------------------------------
          where
            genField :: (Text, ValidSelection) -> Validation DataField
            genField (fieldName, sel) = genFieldD sel
              where
                fieldPath = path <> [fieldName]
                -------------------------------
                genFieldD Selection {selectionAlias = Just aliasFieldName} = do
                  fieldType <- snd <$> lookupFieldType lib fieldPath datatype aliasFieldName
                  pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
                genFieldD _ = do
                  fieldType <- snd <$> lookupFieldType lib fieldPath datatype fieldName
                  pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
            ------------------------------------------------------------------------------------------------------------
            newFieldTypes :: DataFullType -> SelectionSet -> Validation ([[GQLTypeD]], [[Text]])
            newFieldTypes parentType seSet = unzip <$> mapM valSelection seSet
              where
                valSelection selection@(selKey, _) = do
                  let (key, sel) = getSelectionFieldKey selection
                  fieldDatatype <- fst <$> lookupFieldType lib fieldPath parentType key
                  validateSelection fieldDatatype sel
                  --------------------------------------------------------------------
                  where
                    fieldPath = path <> [selKey]
                    --------------------------------------------------------------------
                    validateSelection :: DataFullType -> ValidSelection -> Validation ([GQLTypeD], [Text])
                    validateSelection dType Selection {selectionRec = SelectionField} = do
                      lName <- withLeaf (pure . leafName) dType
                      pure ([], lName)
                    --withLeaf buildLeaf dType
                    validateSelection dType Selection {selectionRec = SelectionSet selectionSet} =
                      genRecordType fieldPath (typeFrom [] dType) dType selectionSet
                    ---- UNION
                    validateSelection dType Selection {selectionRec = UnionSelection unionSelections} = do
                      (tCons, subTypes, requests) <- unzip3 <$> mapM getUnionType unionSelections
                      pure
                        ( GQLTypeD
                            { typeD =
                                TypeD {tNamespace = map unpack fieldPath, tName = unpack $ typeFrom [] dType, tCons}
                            , typeKindD = KindUnion
                            , typeArgD = []
                            } :
                          concat subTypes
                        , concat requests)
                      where
                        getUnionType (selectedTyName, selectionVariant) = do
                          conDatatype <- getType lib selectedTyName
                          genConsD (unpack selectedTyName) conDatatype selectionVariant

scanInputTypes :: DataTypeLib -> Key -> LibUpdater [Key]
scanInputTypes lib name collected
  | name `elem` collected = pure collected
  | otherwise = getType lib name >>= scanType
  where
    scanType (InputObject DataTyCon {typeData}) = resolveUpdates (name : collected) (map toInputTypeD typeData)
      where
        toInputTypeD :: (Text, DataField) -> LibUpdater [Key]
        toInputTypeD (_, DataField {fieldType = TypeAlias {aliasTyCon}}) = scanInputTypes lib aliasTyCon
    scanType (Leaf leaf) = pure (collected <> leafName leaf)
    scanType _ = pure collected

buildInputType :: DataTypeLib -> Text -> Validation [GQLTypeD]
buildInputType lib name = getType lib name >>= subTypes
  where
    subTypes (InputObject DataTyCon {typeName, typeData}) = do
      fields <- traverse toFieldD typeData
      pure
        [ GQLTypeD
            { typeD =
                TypeD
                  { tName = unpack typeName
                  , tNamespace = []
                  , tCons = [ConsD {cName = unpack typeName, cFields = fields}]
                  }
            , typeArgD = []
            , typeKindD = KindInputObject
            }
        ]
          ----------------------------------------------------------------
      where
        toFieldD :: (Text, DataField) -> Validation DataField
        toFieldD (_, field@DataField {fieldType}) = do
          aliasTyCon <- typeFrom [] <$> getType lib (aliasTyCon fieldType)
          pure $ field {fieldType = fieldType {aliasTyCon}}
    subTypes (Leaf (LeafEnum DataTyCon {typeName, typeData})) =
      pure
        [ GQLTypeD
            { typeD = TypeD {tName = unpack typeName, tNamespace = [], tCons = map enumOption typeData}
            , typeArgD = []
            , typeKindD = KindEnum
            }
        ]
      where
        enumOption eName = ConsD {cName = unpack eName, cFields = []}
    subTypes _ = pure []

lookupFieldType :: DataTypeLib -> [Key] -> DataFullType -> Text -> Validation (DataFullType, TypeAlias)
lookupFieldType lib path (OutputObject DataTyCon {typeData}) key =
  case lookup key typeData of
    Just DataField {fieldType = alias@TypeAlias {aliasTyCon}} -> trans <$> getType lib aliasTyCon
      where trans x = (x, alias {aliasTyCon = typeFrom path x, aliasArgs = Nothing})
    Nothing -> Left (compileError key)
lookupFieldType _ _ _ key = Left (compileError key)

getSelectionFieldKey :: (Key, ValidSelection) -> (Key, ValidSelection)
getSelectionFieldKey (_, selection@Selection { selectionAlias = Just name}) = (name, selection)
getSelectionFieldKey sel                                                           = sel

withLeaf :: (DataLeaf -> Validation b) -> DataFullType -> Validation b
withLeaf f (Leaf x) = f x
withLeaf _ _        = Left $ compileError "Invalid schema Expected scalar"

leafName :: DataLeaf -> [Text]
leafName (LeafEnum DataTyCon {typeName}) = [typeName]
leafName _                               = []

getType :: DataTypeLib -> Text -> Validation DataFullType
getType lib typename = lookupType (compileError typename) (allDataTypes lib) typename

typeFromScalar :: Text -> Text
typeFromScalar "Boolean" = "Bool"
typeFromScalar "Int"     = "Int"
typeFromScalar "Float"   = "Float"
typeFromScalar "String"  = "Text"
typeFromScalar "ID"      = "ID"
typeFromScalar _         = "ScalarValue"

typeFrom :: [Key] -> DataFullType -> Text
typeFrom _ (Leaf (BaseScalar x))                      = typeName x
typeFrom _ (Leaf (CustomScalar DataTyCon {typeName})) = typeFromScalar typeName
typeFrom _ (Leaf (LeafEnum x))                        = typeName x
typeFrom _ (InputObject x)                            = typeName x
typeFrom path (OutputObject x)                        = pack $ nameSpaceType path $ typeName x
typeFrom path (Union x)                               = pack $ nameSpaceType path $ typeName x
typeFrom _ (InputUnion x)                             = typeName x
