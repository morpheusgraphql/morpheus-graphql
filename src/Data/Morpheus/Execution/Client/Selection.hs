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

--
-- MORPHEUS
import           Data.Morpheus.Error.Utils                  (globalErrorMessage)
import           Data.Morpheus.Execution.Internal.Utils     (nameSpaceType)
import           Data.Morpheus.Types.Internal.AST.Operation (DefaultValue, Operation (..), ValidOperation,
                                                             Variable (..), VariableDefinitions)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataFullType (..), DataLeaf (..),
                                                             DataTyCon (..), DataTypeLib (..), Key, TypeAlias (..),
                                                             allDataTypes)
import           Data.Morpheus.Types.Internal.DataD         (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Validation.Internal.Utils    (lookupType)

compileError :: Text -> GQLErrors
compileError x = globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

operationTypes :: DataTypeLib -> VariableDefinitions -> ValidOperation -> Validation ([TypeD], [TypeD])
operationTypes lib variables = genOperation
  where
    typeByField :: [Key] -> Text -> DataFullType -> Validation DataFullType
    typeByField path key datatype = fst <$> lookupFieldType path datatype key
    ------------------------------------------------------
    lookupFieldType :: [Key] -> DataFullType -> Text -> Validation (DataFullType, TypeAlias)
    lookupFieldType path (OutputObject DataTyCon {typeData}) key =
      case lookup key typeData of
        Just DataField {fieldType = alias@TypeAlias {aliasTyCon}} -> trans <$> getType lib aliasTyCon
          where trans x = (x, alias {aliasTyCon = typeFrom path x, aliasArgs = Nothing})
        Nothing -> Left (compileError key)
    lookupFieldType _ _ key = Left (compileError key)
    -----------------------------------------------------
    genOperation Operation {operationName, operationSelection} = do
      argTypes <- rootArguments (operationName <> "Args")
      queryTypes <- genRecordType [] operationName queryDataType operationSelection
      pure (argTypes, queryTypes)
      where
        queryDataType = OutputObject $ snd $ query lib
    -------------------------------------------{--}
    genInputType :: Text -> Validation [TypeD]
    genInputType name = getType lib name >>= subTypes
      where
        subTypes (InputObject DataTyCon {typeName, typeData}) = do
          types <- concat <$> mapM toInputTypeD typeData
          fields <- traverse toFieldD typeData
          pure $ typeD fields : types
          where
            typeD fields = TypeD {tName = unpack typeName, tCons = [ConsD {cName = unpack typeName, cFields = fields}]}
            ---------------------------------------------------------------
            toInputTypeD :: (Text, DataField) -> Validation [TypeD]
            toInputTypeD (_, DataField {fieldType}) = genInputType $ aliasTyCon fieldType
            ----------------------------------------------------------------
            toFieldD :: (Text, DataField) -> Validation DataField
            toFieldD (_, field@DataField {fieldType}) = do
              aliasTyCon <- typeFrom [] <$> getType lib (aliasTyCon fieldType)
              pure $ field {fieldType = fieldType {aliasTyCon}}
        subTypes (Leaf x) = buildLeaf x
        subTypes _ = pure []
    -------------------------------------------
    rootArguments :: Text -> Validation [TypeD]
    rootArguments name = do
      types <- concat <$> mapM (genInputType . variableType . snd) variables
      pure $ typeD : types
      where
        typeD :: TypeD
        typeD = TypeD {tName = unpack name, tCons = [ConsD {cName = unpack name, cFields = map fieldD variables}]}
        ---------------------------------------
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
    -------------------------------------------
    getCon path name dataType selectionSet = do
      cFields <- genFields dataType selectionSet
      subTypes <- newFieldTypes (path <> [name]) dataType selectionSet
      pure (ConsD {cName = nameSpaceType path name, cFields}, subTypes)
      ---------------------------------------------------------------------------------------------
      where
        genFields datatype = mapM typeNameFromField
          where
            typeNameFromField :: (Text, Selection) -> Validation DataField
            typeNameFromField (fieldName, Selection {selectionRec = SelectionAlias {aliasFieldName}}) = do
              fieldType <- snd <$> lookupFieldType path datatype aliasFieldName
              pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
            typeNameFromField (fieldName, _) = do
              fieldType <- snd <$> lookupFieldType path datatype fieldName
              pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
    --------------------------------------------
    genRecordType path name dataType selectionSet = do
      (con, subTypes) <- getCon (path <> [name]) name dataType selectionSet
      pure $ TypeD {tName = nameSpaceType path name, tCons = [con]} : subTypes
    ------------------------------------------------------------------------------------------------------------
    newFieldTypes p parentType = fmap concat <$> mapM (validateSelection p)
      where
        validateSelection :: [Key] -> (Key, Selection) -> Validation [TypeD]
        validateSelection path (key, Selection {selectionRec = SelectionField}) =
          typeByField path key parentType >>= buildSelField
          where
            buildSelField (Leaf x) = buildLeaf x
            buildSelField _        = Left $ compileError "Invalid schema Expected scalar"
        validateSelection path (key, Selection {selectionRec = SelectionSet selectionSet}) = do
          datatype <- typeByField path key parentType
          genRecordType path (typeFrom path datatype) datatype selectionSet
        validateSelection n (_, selection@Selection {selectionRec = SelectionAlias {aliasFieldName, aliasSelection}}) =
          validateSelection n (aliasFieldName, selection {selectionRec = aliasSelection})
        validateSelection path (key, Selection {selectionRec = UnionSelection unionSelections}) = do
          unionTypeName <- typeFrom path <$> typeByField path key parentType
          (tCons, subTypes) <- unzip <$> mapM getUnionType unionSelections
          pure $ TypeD {tName = unpack unionTypeName, tCons} : concat subTypes
          where
            getUnionType (typeKey, selSet) = do
              conDatatype <- getType lib typeKey
              getCon path typeKey conDatatype selSet

buildLeaf :: DataLeaf -> Validation [TypeD]
buildLeaf (LeafEnum DataTyCon {typeName, typeData}) =
  pure [TypeD {tName = unpack typeName, tCons = map enumOption typeData}]
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
