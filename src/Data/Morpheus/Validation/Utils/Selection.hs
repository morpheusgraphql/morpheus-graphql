{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Utils.Selection
  ( lookupFieldAsSelectionSet
  , mustBeObject
  , notObject
  , lookupSelectionField
  , lookupPossibleTypes
  , lookupPossibleTypeKeys
  ) where

import           Data.Morpheus.Error.Selection        (cannotQueryField, hasNoSubfields, subfieldsNotSelected)
import           Data.Morpheus.Schema.Internal.AST    (Core (..), Field (..), GObject (..), ObjectField (..),
                                                       OutputObject, TypeLib (..))
import           Data.Morpheus.Schema.TypeKind        (TypeKind (..))
import           Data.Morpheus.Types.Error            (Validation)
import           Data.Morpheus.Types.MetaInfo         (Position)
import           Data.Morpheus.Validation.Utils.Utils (lookupField, lookupType)
import           Data.Text                            (Text)

isObjectKind :: ObjectField -> Bool
isObjectKind (ObjectField _ field') = OBJECT == fieldKind field'

mustBeObject :: (Text, Position) -> ObjectField -> Validation ObjectField
mustBeObject (key', position') field' =
  if isObjectKind field'
    then pure field'
    else Left $ hasNoSubfields key' (fieldType $ fieldContent field') position'

notObject :: (Text, Position) -> ObjectField -> Validation ObjectField
notObject (key', position') field' =
  if isObjectKind field'
    then Left $ subfieldsNotSelected key' (fieldType $ fieldContent field') position'
    else pure field'

lookupPossibleTypeKeys :: Position -> Text -> TypeLib -> ObjectField -> Validation [Text]
lookupPossibleTypeKeys position' key' lib' field' = map fieldType <$> lookupType error' (union lib') type'
  where
    error' = hasNoSubfields key' type' position'
    type' = fieldType $ fieldContent field'

lookupPossibleTypes :: Position -> Text -> TypeLib -> [Text] -> Validation [OutputObject]
lookupPossibleTypes position' key' lib' = mapM (lookupType error' (object lib'))
  where
    error' = hasNoSubfields key' "TODO" position'

lookupFieldAsSelectionSet :: Position -> Text -> TypeLib -> ObjectField -> Validation OutputObject
lookupFieldAsSelectionSet position' key' lib' field' = lookupType error' (object lib') type'
  where
    error' = hasNoSubfields key' type' position'
    type' = fieldType $ fieldContent field'

lookupSelectionField :: Position -> Text -> GObject ObjectField -> Validation ObjectField
lookupSelectionField position' key' (GObject fields' core') = lookupField key' fields' error'
  where
    error' = cannotQueryField key' (name core') position'
