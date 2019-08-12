{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Client.Selection
  ( operationTypes
  ) where

import           Data.Maybe                                 (maybe)
import           Data.Morpheus.Client.Data                  (ConsD (..), FieldD (..), TypeD (..))
import           Data.Morpheus.Error.Internal               (internalUnknownTypeMessage)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator' (..), ValidOperator, Variable (..),
                                                             VariableDefinitions, unpackOperator)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataFullType (..), DataLeaf (..),
                                                             DataType (..), DataTypeLib (..), allDataTypes)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Validation.Utils.Utils       (lookupType)
import           Data.Text                                  (Text, unpack)

compileError :: Text -> GQLErrors
compileError = internalUnknownTypeMessage

operationTypes :: DataTypeLib -> VariableDefinitions -> ValidOperator -> Validation ([TypeD], [TypeD])
operationTypes lib variables = genOp . unpackOperator
  where
    queryDataType = OutputObject $ snd $ query lib
    -----------------------------------------------------
    typeByField :: Text -> DataFullType -> Validation DataFullType
    typeByField key datatype = fieldDataType lib datatype key
    -----------------------------------------------------
    genOp Operator' {operatorName, operatorSelection} = do
      argTypes <- rootArguments (operatorName <> "Args")
      queryTypes <- genRecordType operatorName queryDataType operatorSelection
      pure (argTypes, queryTypes)
    -------------------------------------------{--}
    rootArguments :: Text -> Validation [TypeD]
    rootArguments name = do
      subTypes <- pure [] -- TODO: real inputTypeGeneration
      pure $
        TypeD {tName = unpack name, tCons = [ConsD {cName = unpack name, cFields = map fieldD variables}]} : subTypes
      where
        fieldD :: (Text, Variable ()) -> FieldD
        fieldD (key, Variable {variableType, variableTypeWrappers}) = FieldD (unpack key) ([], unpack variableType)
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
            typeNameFromField (key, _) =
              FieldD (unpack key) . ([], ) . unpack . typeFrom <$> fieldDataType lib datatype key
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

fieldDataType :: DataTypeLib -> DataFullType -> Text -> Validation DataFullType
fieldDataType lib (OutputObject DataType {typeData}) key =
  maybe (Left $ compileError key) (Right . fieldType) (lookup key typeData) >>= getType lib
fieldDataType _ _ key = Left (compileError key)
