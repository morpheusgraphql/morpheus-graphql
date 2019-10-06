{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Client.Selection
  ( operationTypes
  ) where

import           Data.Semigroup                             ((<>))
import           Data.Text                                  (Text, pack, unpack)
import           Debug.Trace

--
-- MORPHEUS
import           Data.Morpheus.Error.Utils                  (globalErrorMessage)
import           Data.Morpheus.Execution.Internal.Utils     (nameSpaceType)
import           Data.Morpheus.Types.Internal.AST.Operation (DefaultValue, Operation (..), ValidOperation,
                                                             Variable (..), VariableDefinitions)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataFullType (..), DataLeaf (..),
                                                             DataTyCon (..), DataTypeLib (..), Key, TypeAlias (..),
                                                             allDataTypes)
import           Data.Morpheus.Types.Internal.DataD         (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Validation.Internal.Utils    (lookupType)

compileError :: Text -> GQLErrors
compileError x = globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

operationTypes :: DataTypeLib -> VariableDefinitions -> ValidOperation -> Validation ([TypeD], [TypeD])
operationTypes lib variables x = traceShow (genOperation x) (genOperation x)
  where
    genOperation Operation {operationName, operationSelection} = do
      argTypes <- rootArguments (operationName <> "Args")
      queryTypes <- genRecordType [] operationName queryDataType operationSelection
      pure (argTypes, queryTypes)
      where
        queryDataType = OutputObject $ snd $ query lib
    -------------------------------------------
    -- generates argument types for Operation Head
    rootArguments :: Text -> Validation [TypeD]
    rootArguments argsName = do
      types <- concat <$> mapM (genInputType . variableType . snd) variables
      pure $ rootArgumentsType : types
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
        ------------------------------------------
        genInputType :: Text -> Validation [TypeD]
        genInputType name = getType lib name >>= subTypes
          where
            subTypes (InputObject DataTyCon {typeName, typeData}) = do
              types <- concat <$> mapM toInputTypeD typeData
              fields <- traverse toFieldD typeData
              pure $ typeD fields : types
              where
                typeD fields =
                  TypeD
                    { tName = unpack typeName
                    , tNamespace = []
                    , tCons = [ConsD {cName = unpack typeName, cFields = fields}]
                    }
                    ---------------------------------------------------------------
                toInputTypeD :: (Text, DataField) -> Validation [TypeD]
                toInputTypeD (_, DataField {fieldType}) = genInputType $ aliasTyCon fieldType
                    ----------------------------------------------------------------
                toFieldD :: (Text, DataField) -> Validation DataField
                toFieldD (_, field@DataField {fieldType}) = do
                  aliasTyCon <- typeFrom [] <$> getType lib (aliasTyCon fieldType)
                  pure $ field {fieldType = fieldType {aliasTyCon}}
            subTypes (Leaf leaf) = buildLeaf leaf
            subTypes _ = pure []
    ---------------------------------------------------------
    -- generates selection Object Types
    genRecordType :: [Key] -> Key -> DataFullType -> SelectionSet -> Validation [TypeD]
    genRecordType path name dataType recordSelSet = do
      (con, subTypes) <- genConsD (unpack name) dataType recordSelSet
      pure $ TypeD {tName, tNamespace = map unpack path, tCons = [con]} : subTypes
      where
        tName = unpack name
        nextPath = path <> [name]
        genConsD :: String -> DataFullType -> SelectionSet -> Validation (ConsD, [TypeD])
        genConsD cName datatype selSet = do
          cFields <- traverse genField selSet
          subTypes <- newFieldTypes datatype selSet
          pure (ConsD {cName, cFields}, subTypes)
          ---------------------------------------------------------------------------------------------
          where
            genField :: (Text, Selection) -> Validation DataField
            genField (fieldName, sel) = genFieldD sel
              where
                fieldPath = nextPath <> [fieldName]
                -------------------------------
                genFieldD Selection {selectionRec = SelectionAlias {aliasFieldName}} = do
                  fieldType <- snd <$> lookupFieldType lib fieldPath datatype aliasFieldName
                  pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
                genFieldD _ = do
                  fieldType <- snd <$> lookupFieldType lib fieldPath datatype fieldName
                  pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
            ------------------------------------------------------------------------------------------------------------
            newFieldTypes parentType = fmap concat <$> mapM valSelection
              where
                valSelection selection@(selKey, _) = do
                  let (key, sel) = getSelectionFieldKey selection
                  fieldDatatype <- fst <$> lookupFieldType lib fieldPath parentType key
                  validateSelection fieldDatatype sel
                  --------------------------------------------------------------------
                  where
                    fieldPath = nextPath <> [selKey]
                    -------------------
                    validateSelection :: DataFullType -> Selection -> Validation [TypeD]
                    validateSelection dType Selection {selectionRec = SelectionField} = withLeaf buildLeaf dType
                    validateSelection dType Selection {selectionRec = SelectionSet selectionSet} =
                      genRecordType fieldPath (typeFrom [] dType) dType selectionSet
                    validateSelection dType aliasSel@Selection {selectionRec = SelectionAlias {aliasSelection}} =
                      validateSelection dType aliasSel {selectionRec = aliasSelection}
                    ---- UNION
                    validateSelection dType Selection {selectionRec = UnionSelection unionSelections} = do
                      (tCons, subTypes) <- unzip <$> mapM getUnionType unionSelections
                      pure $
                        TypeD {tNamespace = map unpack fieldPath, tName = unpack $ typeFrom [] dType, tCons} :
                        concat subTypes
                      where
                        getUnionType (selectedTyName, selectionVariant) = do
                          conDatatype <- getType lib selectedTyName
                          genConsD (unpack selectedTyName) conDatatype selectionVariant

lookupFieldType :: DataTypeLib -> [Key] -> DataFullType -> Text -> Validation (DataFullType, TypeAlias)
lookupFieldType lib path (OutputObject DataTyCon {typeData}) key =
  case lookup key typeData of
    Just DataField {fieldType = alias@TypeAlias {aliasTyCon}} -> trans <$> getType lib aliasTyCon
      where trans x = (x, alias {aliasTyCon = typeFrom path x, aliasArgs = Nothing})
    Nothing -> Left (compileError key)
lookupFieldType _ _ _ key = Left (compileError key)

getSelectionFieldKey :: (Key, Selection) -> (Key, Selection)
getSelectionFieldKey (_, selection@Selection {selectionRec = SelectionAlias {aliasFieldName, aliasSelection}}) =
  (aliasFieldName, selection {selectionRec = aliasSelection})
getSelectionFieldKey sel = sel

withLeaf :: (DataLeaf -> Validation b) -> DataFullType -> Validation b
withLeaf f (Leaf x) = f x
withLeaf _ _        = Left $ compileError "Invalid schema Expected scalar"

buildLeaf :: DataLeaf -> Validation [TypeD]
buildLeaf (LeafEnum DataTyCon {typeName, typeData}) =
  pure [TypeD {tName = unpack typeName, tNamespace = [], tCons = map enumOption typeData}]
  where
    enumOption name = ConsD {cName = unpack name, cFields = []}
buildLeaf _ = pure []

getType :: DataTypeLib -> Text -> Validation DataFullType
getType lib typename = lookupType (compileError typename) (allDataTypes lib) typename

isPrimitive :: Text -> Bool
isPrimitive "Boolean" = True
isPrimitive "Int"     = True
isPrimitive "Float"   = True
isPrimitive "String"  = True
isPrimitive "ID"      = True
isPrimitive _         = False

typeFrom :: [Key] -> DataFullType -> Text
typeFrom _ (Leaf (BaseScalar x)) = typeName x
typeFrom _ (Leaf (CustomScalar DataTyCon {typeName}))
  | isPrimitive typeName = typeName
  | otherwise = "ScalarValue"
typeFrom _ (Leaf (LeafEnum x)) = typeName x
typeFrom _ (InputObject x) = typeName x
typeFrom path (OutputObject x) = pack $ nameSpaceType path $ typeName x
typeFrom path (Union x) = pack $ nameSpaceType path $ typeName x
typeFrom _ (InputUnion x) = typeName x