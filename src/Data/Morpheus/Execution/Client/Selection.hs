{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Client.Selection
  ( operationTypes
  ) where

import           Data.Semigroup                             ((<>))
import           Data.Text                                  (Text, unpack)

--
-- MORPHEUS
import           Data.Morpheus.Error.Utils                  (globalErrorMessage)
import           Data.Morpheus.Types.Internal.AST.Operation (Operation (..), ValidOperation, Variable (..),
                                                             VariableDefinitions)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataFullType (..), DataLeaf (..),
                                                             DataTyCon (..), DataTypeLib (..), TypeAlias (..),
                                                             allDataTypes)
import           Data.Morpheus.Types.Internal.DataD         (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Validation.Internal.Utils    (lookupType)

compileError :: Text -> GQLErrors
compileError x = globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

operationTypes :: DataTypeLib -> VariableDefinitions -> ValidOperation -> Validation ([TypeD], [TypeD])
operationTypes lib variables = genOperation
  where
    queryDataType = OutputObject $ snd $ query lib
    -----------------------------------------------------
    typeByField :: Text -> DataFullType -> Validation DataFullType
    typeByField key datatype = fst <$> lookupFieldType datatype key
    ------------------------------------------------------
    lookupFieldType :: DataFullType -> Text -> Validation (DataFullType, TypeAlias)
    lookupFieldType (OutputObject DataTyCon {typeData}) key =
      case lookup key typeData of
        Just DataField {fieldType = alias@TypeAlias {aliasTyCon}} -> trans <$> getType lib aliasTyCon
          where trans x = (x, alias {aliasTyCon = typeFrom x, aliasArgs = Nothing})
        Nothing -> Left (compileError key)
    lookupFieldType _ key = Left (compileError key)
    -----------------------------------------------------
    genOperation Operation {operationName, operationSelection} = do
      argTypes <- rootArguments (operationName <> "Args")
      queryTypes <- genRecordType operationName queryDataType operationSelection
      pure (argTypes, queryTypes)
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
              aliasTyCon <- typeFrom <$> getType lib (aliasTyCon fieldType)
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
        fieldD :: (Text, Variable ()) -> DataField
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
    getCon name dataType selectionSet = do
      cFields <- genFields dataType selectionSet
      subTypes <- newFieldTypes dataType selectionSet
      pure (ConsD {cName = unpack name, cFields}, subTypes)
      ---------------------------------------------------------------------------------------------
      where
        genFields datatype = mapM typeNameFromField
          where
            typeNameFromField :: (Text, Selection) -> Validation DataField
            typeNameFromField (fieldName, Selection {selectionRec = SelectionAlias {aliasFieldName}}) = do
              fieldType <- snd <$> lookupFieldType datatype aliasFieldName
              pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
            typeNameFromField (fieldName, _) = do
              fieldType <- snd <$> lookupFieldType datatype fieldName
              pure $ DataField {fieldName, fieldArgs = [], fieldArgsType = Nothing, fieldType, fieldHidden = False}
    --------------------------------------------
    genRecordType name dataType selectionSet = do
      (con, subTypes) <- getCon name dataType selectionSet
      pure $ TypeD {tName = unpack name, tCons = [con]} : subTypes
    ------------------------------------------------------------------------------------------------------------
    newFieldTypes parentType = fmap concat <$> mapM validateSelection
      where
        validateSelection :: (Text, Selection) -> Validation [TypeD]
        validateSelection (key, Selection {selectionRec = SelectionField}) =
          key `typeByField` parentType >>= buildSelField
          where
            buildSelField (Leaf x) = buildLeaf x
            buildSelField _        = Left $ compileError "Invalid schema Expected scalar"
        validateSelection (key, Selection {selectionRec = SelectionSet selectionSet}) = do
          datatype <- key `typeByField` parentType
          genRecordType (typeFrom datatype) datatype selectionSet
        validateSelection (_, selection@Selection {selectionRec = SelectionAlias {aliasFieldName, aliasSelection}}) =
          validateSelection (aliasFieldName, selection {selectionRec = aliasSelection})
        validateSelection (key, Selection {selectionRec = UnionSelection unionSelections}) = do
          unionTypeName <- typeFrom <$> key `typeByField` parentType
          (tCons, subTypes) <- unzip <$> mapM getUnionType unionSelections
          pure $ TypeD {tName = unpack unionTypeName, tCons} : concat subTypes
          where
            getUnionType (typeKey, selSet) = do
              conDatatype <- getType lib typeKey
              getCon typeKey conDatatype selSet

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

typeFrom :: DataFullType -> Text
typeFrom (Leaf (BaseScalar x)) = typeName x
typeFrom (Leaf (CustomScalar DataTyCon {typeName}))
  | isPrimitive typeName = typeName
  | otherwise = "ScalarValue"
typeFrom (Leaf (LeafEnum x)) = typeName x
typeFrom (InputObject x) = typeName x
typeFrom (OutputObject x) = typeName x
typeFrom (Union x) = typeName x
typeFrom (InputUnion x) = typeName x
