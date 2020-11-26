{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | associating types to GraphQL Kinds
module Data.Morpheus.Kind
  ( SCALAR,
    OBJECT,
    ENUM,
    WRAPPER,
    UNION,
    INPUT_OBJECT,
    GQL_KIND,
    OUTPUT,
    INPUT,
    INTERFACE,
    ToValue (..),
    isObject,
    TYPE,
  )
where

import Relude

data GQL_KIND
  = SCALAR
  | TYPE
  | WRAPPER
  | INTERFACE

class ToValue (a :: GQL_KIND) where
  toValue :: f a -> GQL_KIND

instance ToValue 'SCALAR where
  toValue _ = SCALAR

instance ToValue 'WRAPPER where
  toValue _ = WRAPPER

instance ToValue 'TYPE where
  toValue _ = TYPE

instance ToValue 'INTERFACE where
  toValue _ = INTERFACE

isObject :: GQL_KIND -> Bool
isObject TYPE = True
isObject INTERFACE = True
isObject _ = False

-- | GraphQL input, type, union , enum
type TYPE = 'TYPE

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
type SCALAR = 'SCALAR

-- | GraphQL interface
type INTERFACE = 'INTERFACE

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'WRAPPER

-- deprecated types

{-# DEPRECATED ENUM "use: deriving(GQLType), will be automatically inferred" #-}

type ENUM = 'TYPE

{-# DEPRECATED OUTPUT "use: deriving(GQLType), will be automatically inferred" #-}

type OUTPUT = 'TYPE

{-# DEPRECATED INPUT "use: deriving(GQLType), will be automatically inferred" #-}

type INPUT = 'TYPE

{-# DEPRECATED INPUT_OBJECT "use: deriving(GQLType), will be automatically inferred" #-}

type INPUT_OBJECT = 'TYPE

{-# DEPRECATED UNION "use: deriving(GQLType), IMPORTANT: only types with <type constructor name><constructor name> will sustain their form, other union constructors will be wrapped inside an new object" #-}

type UNION = 'TYPE

{-# DEPRECATED OBJECT "use: deriving(GQLType), will be automatically inferred" #-}

type OBJECT = 'TYPE
