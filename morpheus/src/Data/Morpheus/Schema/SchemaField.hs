{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Schema.SchemaField
  ( fieldByKey
  , argsByKey
  , wrapAsListType
  ) where

import           Data.List                         (find)
import qualified Data.Morpheus.Schema.GQL__Field   as F (GQL__Field (..))
import qualified Data.Morpheus.Schema.GQL__Type    as T (GQL__Type (..))
import           Data.Morpheus.Types.Introspection (GQL__Field, GQL__InputValue, GQL__Type,
                                                    wrapListType)
import           Data.Morpheus.Types.Types         ((::->) (..))
import           Data.Text                         (Text)

fieldByKey :: Text -> GQL__Type -> Maybe GQL__Field
fieldByKey key gqlType =
  case T.fields gqlType of
    Some fields -> find (\x -> key == F.name x) fields
    _           -> Nothing

argsByKey :: Text -> GQL__Type -> Maybe [GQL__InputValue]
argsByKey key gqlType = F.args <$> fieldByKey key gqlType

wrapAsListType :: GQL__Field -> GQL__Field
wrapAsListType x = x {F._type = wrapListType <$> F._type x}
