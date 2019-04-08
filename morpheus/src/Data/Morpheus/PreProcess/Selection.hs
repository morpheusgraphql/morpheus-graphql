module Data.Morpheus.PreProcess.Selection
  ( lookupFieldAsSelectionSet
  , lookupSelectionObjectFieldType
  , mustBeObject
  , notObject
  ) where

import           Data.Morpheus.Error.Selection       (cannotQueryField, hasNoSubfields, subfieldsNotSelected)
import           Data.Morpheus.PreProcess.Utils      (lookupField, lookupType)
import           Data.Morpheus.Schema.Internal.Types (Core (..), Field (..), GObject (..), ObjectField (..),
                                                      OutputObject, TypeLib (..))
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

lookupFieldAsSelectionSet :: Position -> Text -> TypeLib -> ObjectField -> Validation OutputObject
lookupFieldAsSelectionSet position' key' lib' field' = lookupType error' (object lib') type'
  where
    error' = hasNoSubfields $ MetaInfo {position = position', key = key', typeName = type'}
    type' = fieldType $ fieldContent field'

lookupSelectionField :: Position -> Text -> GObject ObjectField -> Validation ObjectField
lookupSelectionField position' key' (GObject fields' core') = lookupField key' fields' error'
  where
    error' = cannotQueryField $ MetaInfo {position = position', key = key', typeName = name core'}

lookupSelectionObjectFieldType :: Position -> Text -> TypeLib -> GObject ObjectField -> Validation OutputObject
lookupSelectionObjectFieldType position' key' lib' object' =
  lookupSelectionField position' key' object' >>= mustBeObject (key', position') >>=
  lookupFieldAsSelectionSet position' key' lib'
