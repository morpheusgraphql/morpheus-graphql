{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Utils
  ( existsType
  , typeBy
  , fieldOf
  , fieldType
  , differKeys
  ) where

import           Data.List                        ((\\))
import qualified Data.Map                         as M (lookup)
import qualified Data.Morpheus.Schema.Field       as F (name, _type)
import qualified Data.Morpheus.Schema.Type        as T (kind, name, ofType)
import           Data.Morpheus.Schema.TypeKind    (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Field (fieldByKey)
import           Data.Morpheus.Schema.Utils.Utils (Field, Type, TypeLib)
import           Data.Morpheus.Types.Core         (EnhancedKey (..), Key, enhanceKeyWithNull)
import           Data.Morpheus.Types.Describer    (EnumOf (..))
import           Data.Morpheus.Types.Error        (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.MetaInfo     (MetaInfo (..), Position)
import           Data.Text                        as TX (Text)

unwrapType :: Type -> Maybe Type
unwrapType x =
  case T.kind x of
    EnumOf LIST -> T.ofType x
    _           -> Just x

existsType :: (Position, Key) -> TX.Text -> TypeLib -> MetaValidation Type
existsType (position', key') typeName' lib =
  case M.lookup typeName' lib of
    Nothing -> Left $ UnknownType meta
    Just x  -> pure x
  where
    meta = MetaInfo {position = position', typeName = typeName', key = key'}

fieldOf :: Position -> Type -> Text -> MetaValidation Field
fieldOf pos _type fieldName =
  case fieldByKey fieldName _type of
    Nothing    -> Left $ UnknownField meta
    Just field -> pure field
  where
    meta = MetaInfo {key = fieldName, typeName = T.name _type, position = pos}

fieldType :: Position -> TypeLib -> Field -> MetaValidation Type
fieldType position' lib field =
  case F._type field >>= unwrapType of
    Nothing -> Left $ UnknownType $ MetaInfo {key = F.name field, typeName = "", position = position'}
    Just _type -> existsType (position', F.name field) (T.name _type) lib

typeBy :: Position -> TypeLib -> Type -> Text -> MetaValidation Type
typeBy pos lib _parentType _name = fieldOf pos _parentType _name >>= fieldType pos lib

-- pos information can be 0 because we differentiate it to args and it will be not included in error keys
differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys
