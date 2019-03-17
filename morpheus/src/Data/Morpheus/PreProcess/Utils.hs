{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Utils
  ( existsType
  , typeBy
  , fieldOf
  ) where

import           Control.Monad                      ((>=>))
import qualified Data.Map                           as M (lookup)
import           Data.Morpheus.Schema.GQL__Field    as F (name, _type)
import           Data.Morpheus.Schema.GQL__Type     as F (kind)
import qualified Data.Morpheus.Schema.GQL__Type     as T (name, ofType)
import           Data.Morpheus.Schema.GQL__TypeKind (GQL__TypeKind (..))
import           Data.Morpheus.Schema.SchemaField   (fieldByKey)
import           Data.Morpheus.Types.Error          (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.Introspection  (GQLTypeLib, GQL__Field, GQL__Type)
import           Data.Morpheus.Types.MetaInfo       (MetaInfo (..), Position)
import           Data.Morpheus.Types.Types          (EnumOf (..))
import           Data.Text                          as TX (Text)

unwrapType :: GQL__Type -> Maybe GQL__Type
unwrapType x =
  case kind x of
    EnumOf LIST -> T.ofType x
    _           -> Just x

existsType :: TX.Text -> GQLTypeLib -> MetaValidation GQL__Type
existsType _name lib =
  case M.lookup _name lib of
    Nothing -> Left $ UnknownType (MetaInfo {position = 0, typeName = _name, key = ""})
    Just x  -> pure x

fieldOf :: Position -> GQL__Type -> Text -> MetaValidation GQL__Field
fieldOf pos _type fieldName =
  case fieldByKey fieldName _type of
    Nothing    -> Left $ UnknownField meta
    Just field -> pure field
  where
    meta = MetaInfo {key = fieldName, typeName = T.name _type, position = pos}

fieldHasType :: GQL__Field -> Maybe GQL__Type
fieldHasType = F._type >=> unwrapType

fieldType :: Position -> GQLTypeLib -> GQL__Field -> MetaValidation GQL__Type
fieldType pos lib field =
  case fieldHasType field of
    Nothing -> Left $ UnknownType $ MetaInfo {key = F.name field, typeName = "", position = pos}
    Just _type -> existsType (T.name _type) lib

typeBy :: Position -> GQLTypeLib -> GQL__Type -> Text -> MetaValidation GQL__Type
typeBy pos lib _parentType _name = fieldOf pos _parentType _name >>= fieldType pos lib
