{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.Morpheus.Kind.GQLEnum
  ( EnumConstraint
  , decode
  , inputField
  , field
  , introspect
  ) where

import           Data.Morpheus.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), enumTypeOf)
import           Data.Morpheus.Kind.Internal            (ENUM, GQL)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.Types    (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           GHC.Generics

type EnumConstraint a = (Generic a, EnumRep (Rep a), Show a, GQLKind a)

decode :: (Generic a, EnumRep (Rep a)) => Text -> a
decode text = to $ gToEnum text

inputField :: GQLKind a => Proxy a -> Text -> InputField
inputField proxy = InputField . field proxy

field :: GQLKind a => Proxy a -> Text -> Field
field = buildField ENUM

introspect ::
     forall a. (GQLKind a, EnumRep (Rep a))
  => Proxy a
  -> TypeLib
  -> TypeLib
introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

type instance GQL TypeKind = ENUM

type instance GQL DirectiveLocation = ENUM
