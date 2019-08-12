{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Client.Selection
  ( operationTypes
  ) where

import           Data.Maybe                                 (maybe)
import           Data.Morpheus.Client.Data                  (ConsD (..), FieldD (..), TypeD (..))
  --  where
  --  transform (x, y) = (unpack x, map (\(a, b) -> (unpack a, unpack b)) y)

import           Data.Morpheus.Error.Internal               (internalUnknownTypeMessage)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..), ValidOperator)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataFullType (..), DataLeaf (..),
                                                             DataType (..), DataTypeLib (..), allDataTypes)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Validation.Utils.Utils       (lookupType)
import           Data.Text                                  (Text, unpack)

-- operationTypes :: DataTypeLib -> ValidOperator -> Validation [TypeD]
-- operationTypes lib op = getOperatorTypes lib op
operationTypes :: DataTypeLib -> ValidOperator -> Validation [TypeD]
operationTypes lib = genOp . getOp
  where
    getOp (Query x)        = x
    getOp (Mutation x)     = x
    getOp (Subscription x) = x
    -----------------------------------------------
    queryDataType = OutputObject $ snd $ query lib
    -----------------------------------------------------
    typeByField :: Text -> DataFullType -> Validation DataFullType
    typeByField key datatype = fieldDataType lib datatype key
    -----------------------------------------------------
    genOp Operator' {operatorName, operatorSelection} = genRecordType operatorName queryDataType operatorSelection
    -------------------------------------------
    genRecordType name dataType selectionSet = do
      cFields <- genFields dataType selectionSet
      subTypes <- newFieldTypes dataType selectionSet
      pure $ TypeD {tName = unpack name, tCons = [ConsD {cName = unpack name, cFields}]} : subTypes
    -----------------------------
    genFields datatype = mapM typeNameFromField
      where
        typeNameFromField :: (Text, Selection) -> Validation FieldD
        typeNameFromField (key, _) = FieldD (unpack key) . unpack . typeFrom <$> fieldDataType lib datatype key
    ------------------------------
    newFieldTypes parentType = fmap concat <$> mapM validateSelection
      where
        validateSelection :: (Text, Selection) -> Validation [TypeD]
        validateSelection (key, Selection {selectionRec = SelectionSet selectionSet}) = do
          datatype <- key `typeByField` parentType
          genRecordType (typeFrom datatype) datatype selectionSet
        --validateSelection (key, Selection {selectionRec = SelectionField}) = defineEnum <$> key `typeByField` parentType
        --  where
        --        defineEnum :: DataFullType -> [TypeD]
        --        defineEnum (Leaf (LeafEnum x)) = [typeName x, [])]
        --        defineEnum _                   = []
        validateSelection _ = pure []

internalError :: Text -> GQLErrors
internalError x = internalUnknownTypeMessage $ "Missing Type:" <> x

getType :: DataTypeLib -> Text -> Validation DataFullType
getType lib typename = lookupType (internalError typename) (allDataTypes lib) typename

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
  maybe (Left $ internalError key) (Right . fieldType) (lookup key typeData) >>= getType lib
fieldDataType _ _ key = Left (internalError key)
