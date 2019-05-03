{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Kind.Internal
  ( GQLObject
  , GQLScalar
  , introspection
  , PRIMITIVE
  , SCALAR
  , OBJECT
  , ENUM
  , INPUT_OBJECT
  , GQL
  , _introspect
  , _decode
  , GQLConstraint
  , IntrospectionRouter(..)
  ) where

import Data.Morpheus.Schema.Internal.Types (TypeLib)
import Data.Morpheus.Types.Error (Validation(..))
import Data.Morpheus.Types.JSType (JSType(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text, concat, pack)
import GHC.Exts (Constraint)
import Prelude (Eq, Read, Show(..))

-- Hidden GQL type System API
type family GQL a :: *

data PRIMITIVE

data SCALAR

data OBJECT

data ENUM

data INPUT_OBJECT

-- UNION : TODO: implement UNION
type family GQLConstraint a b :: Constraint

class IntrospectionRouter a b where
  routeIntrospection :: Proxy b -> a -> Text
  __introspect :: Proxy b -> Proxy a -> TypeLib -> TypeLib
  __decode :: Proxy b -> JSType -> Validation a

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

introspection ::
     forall a. IntrospectionRouter a (GQL a)
  => a
  -> Text
introspection = routeIntrospection (Proxy @(GQL a))

-- Define GQL Kind Classes
-- Define and bind GQLObject
class GQLObject a where
  introspectObject :: a -> Text
  default introspectObject :: (Show a, Eq a) =>
    a -> Text
  introspectObject x = concat ["Resolved Object :: ", pack (show x)]

--default introInput :: (GQLKind a, Selectors (Rep a) (Text, InputField)) =>
--    Proxy a -> TypeLib -> TypeLib
--  introInput = updateLib (inputObjectOf fields) stack
--    where
--      fieldTypes = getFields (Proxy @(Rep a))
--      stack = map snd fieldTypes
--      fields = map fst fieldTypes
type instance GQLConstraint a OBJECT = GQLObject a

instance GQLObject a => IntrospectionRouter a OBJECT where
  routeIntrospection _ = introspectObject

-- Define and bind GQLScalar
class GQLScalar a where
  introspectScalar :: a -> Text
  default introspectScalar :: (Show a, Read a) =>
    a -> Text
  introspectScalar x = concat ["Resolved Scalar:: ", pack (show x)]

type instance GQLConstraint a SCALAR = GQLScalar a

instance GQLScalar a => IntrospectionRouter a SCALAR where
  routeIntrospection _ = introspectScalar