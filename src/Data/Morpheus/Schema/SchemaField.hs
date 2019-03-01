{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus.Schema.SchemaField
    ( selectFieldByKey
    , getFieldTypeByKey
    , fieldArgsByKey
    , wrapAsListType
    )
where

import           Data.List                      ( find )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text(..) )
import           Data.Morpheus.Types.Types      ( (::->)(..) )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Field
                                                , GQL__InputValue
                                                , GQL__Type
                                                , wrapListType
                                                , unwrapType
                                                )
import           Control.Monad                  ( join )
import qualified Data.Morpheus.Schema.GQL__Field
                                               as F
                                                ( GQL__Field(..) )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
                                                ( GQL__Type(..) )

selectFieldByKey :: Text -> GQL__Type -> Maybe GQL__Field
selectFieldByKey key gqlType = case T.fields gqlType of
    Some fields -> find (\x -> key == F.name x) fields
    _           -> Nothing

getFieldTypeByKey :: Text -> GQL__Type -> Maybe GQL__Type
getFieldTypeByKey key gqlType =
    selectFieldByKey key gqlType >>= F._type >>= unwrapType

fieldArgsByKey :: Text -> GQL__Type -> Maybe [GQL__InputValue]
fieldArgsByKey key gqlType = F.args <$> selectFieldByKey key gqlType


wrapAsListType :: GQL__Field -> GQL__Field
wrapAsListType x = x { F._type = wrapListType <$> F._type x }

