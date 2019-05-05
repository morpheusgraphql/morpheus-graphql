{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.Morpheus.Kind.GQLEnum
  ( GQLEnum(..)
  , EnumConstraint
  ) where

import           Data.Morpheus.Generics.GDecodeEnum     (GDecodeEnum (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), enumTypeOf)
import           Data.Morpheus.Kind.Internal            (ENUM, GQL)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.Types    (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           GHC.Generics

type EnumConstraint a = (GQLEnum a, Generic a, GDecodeEnum (Rep a), Show a, GQLKind a)

class GQLEnum a where
  decode :: (Generic a, GDecodeEnum (Rep a)) => Text -> a
  decode text = to $ gToEnum text
  inputField :: GQLKind a => Proxy a -> Text -> InputField
  inputField proxy = InputField . field proxy
  field :: GQLKind a => Proxy a -> Text -> Field
  field = buildField ENUM
  introspect :: (GQLKind a, GDecodeEnum (Rep a)) => Proxy a -> TypeLib -> TypeLib
  introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

instance GQLEnum TypeKind

instance GQLEnum DirectiveLocation

type instance GQL TypeKind = ENUM

type instance GQL DirectiveLocation = ENUM
