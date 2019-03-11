{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Schema.SchemaField
  ( selectFieldByKey
  , getFieldTypeByKey
  , fieldArgsByKey
  , wrapAsListType
  ) where

import           Control.Monad                     (join)
import           Data.Data                         (Data)
import           Data.List                         (find)
import qualified Data.Morpheus.Schema.GQL__Field   as F (GQL__Field (..))
import qualified Data.Morpheus.Schema.GQL__Type    as T (GQL__Type (..))
import           Data.Morpheus.Types.Introspection (GQL__Field, GQL__InputValue,
                                                    GQL__Type, unwrapType,
                                                    wrapListType)
import           Data.Morpheus.Types.Types         ((::->) (..))
import           Data.Text                         (Text (..))

selectFieldByKey :: Text -> GQL__Type -> Maybe GQL__Field
selectFieldByKey key gqlType =
  case T.fields gqlType of
    Some fields -> find (\x -> key == F.name x) fields
    _           -> Nothing

getFieldTypeByKey :: Text -> GQL__Type -> Maybe GQL__Type
getFieldTypeByKey key gqlType = selectFieldByKey key gqlType >>= F._type >>= unwrapType

fieldArgsByKey :: Text -> GQL__Type -> Maybe [GQL__InputValue]
fieldArgsByKey key gqlType = F.args <$> selectFieldByKey key gqlType

wrapAsListType :: GQL__Field -> GQL__Field
wrapAsListType x = x {F._type = wrapListType <$> F._type x}
