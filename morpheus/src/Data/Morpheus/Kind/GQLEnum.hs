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
import           Data.Morpheus.Schema.Field             (createFieldWith)
import           Data.Morpheus.Schema.InputValue        (createInputValueWith)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, TypeLib)
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           GHC.Generics

class GQLEnum a where
  decode :: Text -> a
  default decode :: (Generic a, GDecodeEnum (Rep a)) =>
    Text -> a
  decode text = to $ gToEnum text
  asType :: GQLKind a => Proxy a -> T.Text -> InputValue
  asType proxy name = createInputValueWith name (enumTypeOf [] proxy)
  asField :: GQLKind a => Proxy a -> T.Text -> Field
  asField proxy name = createFieldWith name (enumTypeOf [] proxy) []
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (GQLKind a, GDecodeEnum (Rep a)) =>
    Proxy a -> TypeLib -> TypeLib
  introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

instance GQLEnum TypeKind

instance GQLEnum DirectiveLocation
