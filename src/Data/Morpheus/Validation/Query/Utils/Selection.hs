{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Utils.Selection
  ( lookupFieldAsSelectionSet
  , lookupSelectionField
  , lookupUnionTypes
  ) where

import           Data.Morpheus.Error.Selection           (cannotQueryField, hasNoSubfields)
import           Data.Morpheus.Types.Internal.Base       (Position)
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataObject, DataTyCon (..), DataTypeLib (..),
                                                          TypeAlias (..), lookupDataObject, lookupDataUnion )
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Validation.Internal.Utils (lookupField)
import           Data.Text                               (Text)

lookupUnionTypes :: Position -> Text -> DataTypeLib -> DataField -> Validation [DataObject]
lookupUnionTypes position key lib DataField {fieldType = TypeAlias {aliasTyCon = typeName}} =
  lookupDataUnion gqlError typeName lib >>= mapM (flip (lookupDataObject gqlError) lib . aliasTyCon . fieldType) . typeData
  where
    gqlError = hasNoSubfields key typeName position

lookupFieldAsSelectionSet :: Position -> Text -> DataTypeLib -> DataField -> Validation DataObject
lookupFieldAsSelectionSet position key lib DataField {fieldType = TypeAlias {aliasTyCon}} =
  lookupDataObject gqlError aliasTyCon lib
  where
    gqlError = hasNoSubfields key aliasTyCon position

lookupSelectionField :: Position -> Text -> DataObject -> Validation DataField
lookupSelectionField position' key' DataTyCon {typeData = fields', typeName = name'} = lookupField key' fields' error'
  where
    error' = cannotQueryField key' name' position'
