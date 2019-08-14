{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Client.Selection
  ( operationTypes
  ) where

import           Data.Morpheus.Client.Data                  (ConsD (..), FieldD (..), TypeD (..), gqlToHSWrappers)
import           Data.Morpheus.Error.Internal               (internalUnknownTypeMessage)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator' (..), ValidOperator, Variable (..),
                                                             VariableDefinitions, unpackOperator)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataFullType (..), DataLeaf (..),
                                                             DataType (..), DataTypeLib (..), DataTypeWrapper,
                                                             allDataTypes)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Validation.Utils.Utils       (lookupType)
import           Data.Text                                  (Text, unpack)

compileError :: Text -> GQLErrors
compileError x = internalUnknownTypeMessage $ " \"" <> x <> "\" ;"

operationTypes :: DataTypeLib -> VariableDefinitions -> ValidOperator -> Validation ([TypeD], [TypeD])
operationTypes lib variables = genOp . unpackOperator
  where
    queryDataType = OutputObject $ snd $ query lib
    -----------------------------------------------------
    typeByField :: Text -> DataFullType -> Validation DataFullType
    typeByField key datatype = fst <$> fieldDataType datatype key
    ------------------------------------------------------
    fieldDataType :: DataFullType -> Text -> Validation (DataFullType, [DataTypeWrapper])
    fieldDataType (OutputObject DataType {typeData}) key =
      case lookup key typeData of
        Just DataField {fieldTypeWrappers, fieldType} -> trans <$> getType lib fieldType
          where trans x = (x, fieldTypeWrappers)
        Nothing -> Left (compileError key)
    fieldDataType _ key = Left (compileError key)
    -----------------------------------------------------
    genOp Operator' {operatorName, operatorSelection} = do
      argTypes <- rootArguments (operatorName <> "Args")
      queryTypes <- genRecordType operatorName queryDataType operatorSelection
      pure (argTypes, queryTypes)
    -------------------------------------------{--}
    genInputType :: Text -> Validation [TypeD]
    genInputType name = getType lib name >>= subTypes
      where
        subTypes (InputObject DataType {typeName, typeData}) = do
          types <- concat <$> mapM toInputTypeD typeData
          pure $ typeD : types
          where
            typeD =
              TypeD
                {tName = unpack typeName, tCons = [ConsD {cName = unpack typeName, cFields = map toFieldD typeData}]}
            ---------------------------------------------------------------
            toInputTypeD :: (Text, DataField a) -> Validation [TypeD]
            toInputTypeD (_, DataField {fieldType}) = genInputType fieldType
            ----------------------------------------------------------------
            toFieldD :: (Text, DataField a) -> FieldD
            toFieldD (key, DataField {fieldType, fieldTypeWrappers}) = FieldD (unpack key) wrType
              where
                wrType = gqlToHSWrappers fieldTypeWrappers (unpack fieldType)
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
        fieldD :: (Text, Variable ()) -> FieldD
        fieldD (key, Variable {variableType, variableTypeWrappers}) = FieldD (unpack key) wrType
          where
            wrType = gqlToHSWrappers variableTypeWrappers (unpack variableType)
    -------------------------------------------
    genRecordType name dataType selectionSet = do
      cFields <- genFields dataType selectionSet
      subTypes <- newFieldTypes dataType selectionSet
      pure $ TypeD {tName = unpack name, tCons = [ConsD {cName = unpack name, cFields}]} : subTypes
      ---------------------------------------------------------------------------------------------
      where
        genFields datatype = mapM typeNameFromField
          where
            typeNameFromField :: (Text, Selection) -> Validation FieldD
            typeNameFromField (key, _) = FieldD (unpack key) <$> wrType
              where
                wrType = do
                  (newType, wrappers) <- fieldDataType datatype key
                  pure $ gqlToHSWrappers wrappers (unpack $ typeFrom newType)
    ------------------------------------------------------------------------------------------------------------
    newFieldTypes parentType = fmap concat <$> mapM validateSelection
      where
        validateSelection :: (Text, Selection) -> Validation [TypeD]
        validateSelection (key, Selection {selectionRec = SelectionSet selectionSet}) = do
          datatype <- key `typeByField` parentType
          genRecordType (typeFrom datatype) datatype selectionSet
        validateSelection _ = pure []

getType :: DataTypeLib -> Text -> Validation DataFullType
getType lib typename = lookupType (compileError typename) (allDataTypes lib) typename

typeFrom :: DataFullType -> Text
typeFrom (Leaf (BaseScalar x))   = typeName x
typeFrom (Leaf (CustomScalar _)) = "String"
typeFrom (Leaf (LeafEnum x))     = typeName x
typeFrom (InputObject x)         = typeName x
typeFrom (OutputObject x)        = typeName x
typeFrom (Union x)               = typeName x
typeFrom (InputUnion x)          = typeName x
