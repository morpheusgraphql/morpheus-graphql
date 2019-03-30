{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}

module Data.Morpheus.Kind.GQLEnum
  ( GQLEnum(..)
  ) where

import           Data.Morpheus.Generics.GDecodeEnum     (GDecodeEnum (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), enumTypeOf)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.Types    (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           GHC.Generics

class GQLEnum a where
  decode :: Text -> a
  default decode :: (Generic a, GDecodeEnum (Rep a)) =>
    Text -> a
  decode text = to $ gToEnum text
  asInputField :: Proxy a -> Text -> InputField
  default asInputField :: GQLKind a =>
    Proxy a -> Text -> InputField
  asInputField proxy = InputField . asField proxy
  asField :: Proxy a -> Text -> Field
  default asField :: GQLKind a =>
    Proxy a -> Text -> Field
  asField proxy name = Field {fieldName = name, notNull = True, kind = ENUM, fieldType = typeID proxy}
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (GQLKind a, GDecodeEnum (Rep a)) =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

instance GQLEnum TypeKind

instance GQLEnum DirectiveLocation
