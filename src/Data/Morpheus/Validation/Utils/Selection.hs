module Data.Morpheus.Validation.Utils.Selection
  ( lookupFieldAsSelectionSet
  , lookupSelectionField
  , lookupUnionTypes
  ) where

import           Data.Morpheus.Error.Selection           (cannotQueryField, hasNoSubfields)
import           Data.Morpheus.Types.Internal.Base       (Position)
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataOutputField, DataOutputObject,
                                                          DataType (..), DataTypeLib (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Validation.Utils.Utils    (lookupField, lookupType)
import           Data.Text                               (Text)

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
