module Data.Morpheus.Validation.Utils.Selection
  ( lookupFieldAsSelectionSet
  , mustBeObject
  , notObject
  , lookupSelectionField
  , lookupUnionTypes
  ) where

import           Data.Morpheus.Error.Selection           (cannotQueryField, hasNoSubfields, subfieldsNotSelected)
import           Data.Morpheus.Schema.TypeKind           (TypeKind (..))
import           Data.Morpheus.Types.Internal.Base       (Position)
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataOutputField, DataOutputObject,
                                                          DataType (..), DataTypeLib (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Validation.Utils.Utils    (lookupField, lookupType)
import           Data.Text                               (Text)

isObjectKind :: DataField a -> Bool
isObjectKind field' = OBJECT == fieldKind field'

mustBeObject :: (Text, Position) -> DataOutputField -> Validation DataOutputField
mustBeObject (key', position') field' =
  if isObjectKind field'
    then pure field'
    else Left $ hasNoSubfields key' (fieldType field') position'

notObject :: (Text, Position) -> DataOutputField -> Validation DataOutputField
notObject (key', position') field' =
  if isObjectKind field'
    then Left $ subfieldsNotSelected key' (fieldType field') position'
    else pure field'

lookupUnionTypes :: Position -> Text -> DataTypeLib -> DataOutputField -> Validation [DataOutputObject]
lookupUnionTypes position' key' lib' DataField {fieldType = type'} =
  lookupType error' (union lib') type' >>= mapM (lookupType error' (object lib') . fieldType) . typeData
  where
    error' = hasNoSubfields key' type' position'

lookupFieldAsSelectionSet :: Position -> Text -> DataTypeLib -> DataOutputField -> Validation DataOutputObject
lookupFieldAsSelectionSet position' key' lib' DataField {fieldType = type'} = lookupType error' (object lib') type'
  where
    error' = hasNoSubfields key' type' position'

lookupSelectionField :: Position -> Text -> DataOutputObject -> Validation DataOutputField
lookupSelectionField position' key' DataType {typeData = fields', typeName = name'} = lookupField key' fields' error'
  where
    error' = cannotQueryField key' name' position'
