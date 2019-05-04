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

class IntrospectionRouter a b where
  __introspect :: Proxy b -> Proxy a -> TypeLib -> TypeLib
  __decode :: Proxy b -> JSType -> Validation a
  __encode :: Proxy b -> (Text, Selection) -> a -> ResolveIO JSType
  __field :: Proxy b -> Proxy a -> Text -> InputField

--instance IntrospectionRouter a b => IntrospectionRouter (Maybe a) b where
--  __introspect proxyB _ = __introspect proxyB (Proxy @a)
--  __decode proxy value = Just <$> __decode proxy value
--  __encode proxyB sel (Just x) = __encode proxyB sel x
--  __encode _ _ Nothing = pure JSNull
--  __field proxyB _ = __field proxyB (Proxy @a)
-- TODO: Remove It after finishing
--instance IntrospectionRouter a SCALAR => IntrospectionRouter (ScalarOf a) SCALAR where
--  __introspect proxyB _ = __introspect proxyB (Proxy @a)
--  __decode proxy value = ScalarOf <$> __decode proxy value
--  __encode proxyB sel (ScalarOf x) = __encode proxyB sel x
--  __field proxyB _ = __field proxyB (Proxy @a)
_field ::
     forall a. IntrospectionRouter a (GQL a)
  => Proxy a
  -> Text
  -> InputField
_field = __field (Proxy @(GQL a))

_encode ::
     forall a. IntrospectionRouter a (GQL a)
  => (Text, Selection)
  -> a
  -> ResolveIO JSType
_encode = __encode (Proxy @(GQL a))

_decode ::
     forall a. IntrospectionRouter a (GQL a)
  => JSType
  -> Validation a
_decode = __decode (Proxy @(GQL a))

_introspect ::
     forall a. IntrospectionRouter a (GQL a)
  => Proxy a
  -> TypeLib
  -> TypeLib
_introspect = __introspect (Proxy @(GQL a))
--introspection ::
--     forall a. IntrospectionRouter a (GQL a)
--  => a
--  -> Text
--introspection = routeIntrospection (Proxy @(GQL a))
-- Define GQL Kind Classes
-- Define and bind GQLObject
--class GQLObject a where
--  introspectObject :: a -> Text
--  default introspectObject :: (Show a, Eq a) =>
--    a -> Text
--  introspectObject x = concat ["Resolved Object :: ", pack (show x)]
--default introInput :: (GQLKind a, Selectors (Rep a) (Text, InputField)) =>
--    Proxy a -> TypeLib -> TypeLib
--  introInput = updateLib (inputObjectOf fields) stack
--    where
--      fieldTypes = getFields (Proxy @(Rep a))
--      stack = map snd fieldTypes
--      fields = map fst fieldTypes
-- type instance GQLConstraint a OBJECT = GQLObject a
--instance GQLObject a => IntrospectionRouter a OBJECT where
--  routeIntrospection _ = introspectObject
-- Define and bind GQLScalar
--class GQLScalar a where
--  introspectScalar :: a -> Text
--  default introspectScalar :: (Show a, Read a) =>
--    a -> Text
--  introspectScalar x = concat ["Resolved Scalar:: ", pack (show x)]
--type instance GQLConstraint a SCALAR = GQLScalar a
--instance GQLScalar a => IntrospectionRouter a SCALAR where
--  routeIntrospection _ = introspectScalar