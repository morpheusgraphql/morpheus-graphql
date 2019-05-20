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
import           Data.Morpheus.Schema.TypeKind        (TypeKind (..))
import           Data.Morpheus.Types.Error            (Validation)
import           Data.Morpheus.Types.Internal.AST     (ASTField (..), ASTOutputField, ASTOutputObject, ASTType (..),
                                                       ASTTypeLib (..))
import           Data.Morpheus.Types.MetaInfo         (Position)
import           Data.Morpheus.Validation.Utils.Utils (lookupField, lookupType)
import           Data.Text                            (Text)

isObjectKind :: ASTField a -> Bool
isObjectKind field' = OBJECT == fieldKind field'

mustBeObject :: (Text, Position) -> ASTOutputField -> Validation ASTOutputField
mustBeObject (key', position') field' =
  if isObjectKind field'
    then pure field'
    else Left $ hasNoSubfields key' (fieldType field') position'

notObject :: (Text, Position) -> ASTOutputField -> Validation ASTOutputField
notObject (key', position') field' =
  if isObjectKind field'
    then Left $ subfieldsNotSelected key' (fieldType field') position'
    else pure field'

lookupPossibleTypeKeys :: Position -> Text -> ASTTypeLib -> ASTOutputField -> Validation [Text]
lookupPossibleTypeKeys position' key' lib' ASTField {fieldType = type'} =
  map fieldType <$> lookupType error' (union lib') type'
  where
    error' = hasNoSubfields key' type' position'

lookupPossibleTypes :: Position -> Text -> ASTTypeLib -> [Text] -> Validation [ASTOutputObject]
lookupPossibleTypes position' key' lib' = mapM (lookupType error' (object lib'))
  where
    error' = hasNoSubfields key' "TODO" position'

lookupFieldAsSelectionSet :: Position -> Text -> ASTTypeLib -> ASTOutputField -> Validation ASTOutputObject
lookupFieldAsSelectionSet position' key' lib' ASTField {fieldType = type'} = lookupType error' (object lib') type'
  where
    error' = hasNoSubfields key' type' position'

lookupSelectionField :: Position -> Text -> ASTOutputObject -> Validation ASTOutputField
lookupSelectionField position' key' ASTType {typeData = fields', typeName = name'} = lookupField key' fields' error'
  where
    error' = cannotQueryField key' name' position'
