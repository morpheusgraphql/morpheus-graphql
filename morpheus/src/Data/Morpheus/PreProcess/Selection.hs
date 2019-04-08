module Data.Morpheus.PreProcess.Selection
  ( lookupFieldAsSelectionSet
  , lookupFieldSelectionSetType
  , mustBeObject
  , notObject
  ) where

import           Data.Morpheus.Error.Selection       (hasNoSubfields, subfieldsNotSelected)
import           Data.Morpheus.PreProcess.Utils      (lookupType)
import           Data.Morpheus.Schema.Internal.Types (Field (..), ObjectField (..), OutputObject, TypeLib (..))
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           (Text)

isObjectKind :: ObjectField -> Bool
isObjectKind (ObjectField _ field') = OBJECT == kind field'

mustBeObject :: (Text, Position) -> ObjectField -> Validation ObjectField
mustBeObject (key', position') field' =
  if isObjectKind field'
    then pure field'
    else Left $ hasNoSubfields meta
  where
    meta = MetaInfo {position = position', key = key', typeName = fieldType $ fieldContent field'}

notObject :: (Text, Position) -> ObjectField -> Validation ObjectField
notObject (key', position') field' =
  if isObjectKind field'
    then Left $ subfieldsNotSelected meta
    else pure field'
  where
    meta = MetaInfo {position = position', key = key', typeName = fieldType $ fieldContent field'}

lookupFieldAsSelectionSet :: Position -> Text -> ObjectField -> TypeLib -> Validation OutputObject
lookupFieldAsSelectionSet position' key' field' lib' = lookupType error' (object lib') type'
  where
    error' = hasNoSubfields $ MetaInfo {position = position', key = key', typeName = type'}
    type' = fieldType $ fieldContent field'

lookupFieldSelectionSetType :: Position -> Text -> ObjectField -> TypeLib -> Validation OutputObject
lookupFieldSelectionSetType position' key' field' lib' = lookupType error' (object lib') type'
  where
    error' = subfieldsNotSelected $ MetaInfo {position = position', key = key', typeName = type'}
    type' = fieldType $ fieldContent field'
