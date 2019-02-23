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
                                                ( GQL__Field(..)
                                                , GQL__Type(fields)
                                                , GQL__InputValue
                                                , wrapListType
                                                , unwrapType
                                                )
import           Control.Monad                  ( join )


selectFieldByKey :: Text -> GQL__Type -> Maybe GQL__Field
selectFieldByKey key gqlType = case fields gqlType of
    Some fields -> find (\x -> key == name x) fields
    _           -> Nothing

getFieldTypeByKey :: Text -> GQL__Type -> Maybe GQL__Type
getFieldTypeByKey key gqlType = selectFieldByKey key gqlType >>= _type >>= unwrapType

fieldArgsByKey :: Text -> GQL__Type -> Maybe [GQL__InputValue]
fieldArgsByKey key gqlType = args <$> selectFieldByKey key gqlType


wrapAsListType :: GQL__Field -> GQL__Field
wrapAsListType x = x { _type = wrapListType <$> _type x }

