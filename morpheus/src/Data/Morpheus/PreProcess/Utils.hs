{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Utils
  ( existsType
  , typeBy
  , fieldOf
  , fieldType
  ) where

import qualified Data.Map                         as M (lookup)
import qualified Data.Morpheus.Schema.Field       as F (name, _type)
import           Data.Morpheus.Schema.Helpers     (Field, Type, TypeLib)
import           Data.Morpheus.Schema.SchemaField (fieldByKey)
import qualified Data.Morpheus.Schema.Type        as T (kind, name, ofType)
import           Data.Morpheus.Schema.TypeKind    (TypeKind (..))
import           Data.Morpheus.Types.Error        (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.MetaInfo     (MetaInfo (..), Position)
import           Data.Morpheus.Types.Types        (EnumOf (..))
import           Data.Text                        as TX (Text)

unwrapType :: Type -> Maybe Type
unwrapType x =
  case T.kind x of
    EnumOf LIST -> T.ofType x
    _           -> Just x

existsType :: TX.Text -> TypeLib -> MetaValidation Type
existsType _name lib =
  case M.lookup _name lib of
    Nothing -> Left $ UnknownType (MetaInfo {position = 0, typeName = _name, key = ""})
    Just x  -> pure x

fieldOf :: Position -> Type -> Text -> MetaValidation Field
fieldOf pos _type fieldName =
  case fieldByKey fieldName _type of
    Nothing    -> Left $ UnknownField meta
    Just field -> pure field
  where
    meta = MetaInfo {key = fieldName, typeName = T.name _type, position = pos}

fieldType :: Position -> TypeLib -> Field -> MetaValidation Type
fieldType pos lib field =
  case F._type field >>= unwrapType of
    Nothing -> Left $ UnknownType $ MetaInfo {key = F.name field, typeName = "", position = pos}
    Just _type -> existsType (T.name _type) lib

typeBy :: Position -> TypeLib -> Type -> Text -> MetaValidation Type
typeBy pos lib _parentType _name = fieldOf pos _parentType _name >>= fieldType pos lib
