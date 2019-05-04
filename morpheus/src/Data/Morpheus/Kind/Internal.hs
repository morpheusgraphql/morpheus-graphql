{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Morpheus.Kind.Internal
  ( PRIMITIVE
  , SCALAR
  , OBJECT
  , ENUM
  , INPUT_OBJECT
  , GQL
  , _introspect
  , _decode
  , _field
  , _encode
  , GQLConstraint
  , IntrospectionRouter(..)
  ) where

import Data.Morpheus.Schema.Internal.Types (InputField, TypeLib)
import Data.Morpheus.Types.Error (ResolveIO, Validation)
import Data.Morpheus.Types.JSType (JSType(..))
import Data.Morpheus.Types.Query.Selection (Selection)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Exts (Constraint)

-- Hidden GQL type System API
type family GQL a :: *

-- default Type Instances
type instance GQL Text = PRIMITIVE

type instance GQL Int = PRIMITIVE

type instance GQL Float = PRIMITIVE

type instance GQL Bool = PRIMITIVE

type instance GQL (Maybe a) = GQL a

type instance GQL [a] = GQL a

-- default Data Kinds
data PRIMITIVE

data SCALAR

data OBJECT

data ENUM

data INPUT_OBJECT

type family GQLConstraint a b :: Constraint

-- IntrospectionRouter class
type Intro_ a = Proxy a -> TypeLib -> TypeLib

type Decode_ a = JSType -> Validation a

type Encode_ a = (Text, Selection) -> a -> ResolveIO JSType

type Field_ a = Proxy a -> Text -> InputField

class IntrospectionRouter a b where
  __introspect :: Proxy b -> Intro_ a
  __decode :: Proxy b -> Decode_ a
  __encode :: Proxy b -> Encode_ a
  __field :: Proxy b -> Field_ a

_field ::
     forall a. IntrospectionRouter a (GQL a)
  => Field_ a
_field = __field (Proxy @(GQL a))

_encode ::
     forall a. IntrospectionRouter a (GQL a)
  => Encode_ a
_encode = __encode (Proxy @(GQL a))

_decode ::
     forall a. IntrospectionRouter a (GQL a)
  => Decode_ a
_decode = __decode (Proxy @(GQL a))

_introspect ::
     forall a. IntrospectionRouter a (GQL a)
  => Intro_ a
_introspect = __introspect (Proxy @(GQL a))