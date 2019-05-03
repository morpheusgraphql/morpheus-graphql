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
  , SCALAR
  , OBJECT
  , ENUM
  , INPUT_OBJECT
  , GQL
  ) where

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