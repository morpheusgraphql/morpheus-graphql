module Data.Morpheus.PreProcess.Selection where

import           Data.Morpheus.Error.Selection       (subfieldsNotSelected)
import           Data.Morpheus.PreProcess.Utils      (lookupType)
import           Data.Morpheus.Schema.Internal.Types (Field (..), ObjectField (..), OutputObject, TypeLib (..))
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           (Text)

lookupFieldAsSelectionSet :: Position -> Text -> ObjectField -> TypeLib -> Validation OutputObject
lookupFieldAsSelectionSet position' key' field' lib' = lookupType error' (object lib') type'
  where
    error' = subfieldsNotSelected meta'
    meta' = MetaInfo {position = position', key = key', typeName = type'}
    type' = fieldType $ fieldContent field'
