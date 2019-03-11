{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Utils
  ( existsType
  , typeBy
  , fieldOf
  ) where

import qualified Data.Map                          as M (lookup)
import           Data.Morpheus.ErrorMessage        (cannotQueryField,
                                                    handleError)
import qualified Data.Morpheus.Schema.GQL__Type    as T
import           Data.Morpheus.Schema.SchemaField  (getFieldTypeByKey,
                                                    selectFieldByKey)
import           Data.Morpheus.Types.Introspection (GQLTypeLib, GQL__Field,
                                                    GQL__Type)
import           Data.Morpheus.Types.MetaInfo      (MetaInfo (..), Position)
import           Data.Morpheus.Types.Types         (Validation)
import           Data.Text                         as TX (Text, concat)

fieldTypeOf :: Position -> GQL__Type -> Text -> Validation GQL__Type
fieldTypeOf pos _type fieldName =
  case getFieldTypeByKey fieldName _type of
    Nothing        -> Left $ cannotQueryField meta
    Just fieldType -> pure fieldType
  where
    meta = MetaInfo {key = fieldName, typeName = T.name _type, position = pos}

typeBy pos typeLib _parentType _name = fieldTypeOf pos _parentType _name >>= fieldType
  where
    fieldType field = existsType (T.name field) typeLib

existsType :: TX.Text -> GQLTypeLib -> Validation GQL__Type
existsType typeName typeLib =
  case M.lookup typeName typeLib of
    Nothing -> handleError $ TX.concat ["type does not exist", typeName]
    Just x  -> pure x

fieldOf :: Position -> GQL__Type -> Text -> Validation GQL__Field
fieldOf pos _type fieldName =
  case selectFieldByKey fieldName _type of
    Nothing    -> Left $ cannotQueryField meta
    Just field -> pure field
  where
    meta = MetaInfo {key = fieldName, typeName = T.name _type, position = pos}
