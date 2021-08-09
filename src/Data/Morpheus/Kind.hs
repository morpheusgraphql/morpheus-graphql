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
    DerivingKind (..),
    GQL_KIND,
    OUTPUT,
    INPUT,
    INTERFACE,
    ToValue (..),
    isObject,
    TYPE,
    CUSTOM,
  )
where

import Relude

{-# DEPRECATED GQL_KIND "use: DerivingKind" #-}

type GQL_KIND = DerivingKind

data DerivingKind
  = SCALAR
  | TYPE
  | WRAPPER
  | CUSTOM
  deriving (Show)

class ToValue (a :: DerivingKind) where
  toValue :: f a -> DerivingKind

instance ToValue 'SCALAR where
  toValue _ = SCALAR

instance ToValue 'WRAPPER where
  toValue _ = WRAPPER

instance ToValue 'TYPE where
  toValue _ = TYPE

instance ToValue 'CUSTOM where
  toValue _ = CUSTOM

isObject :: DerivingKind -> Bool
isObject TYPE = True
isObject _ = False

-- | GraphQL input, type, union , enum
type TYPE = 'TYPE

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
type SCALAR = 'SCALAR

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'WRAPPER

type CUSTOM = 'CUSTOM

-- deprecated types

{-# DEPRECATED INTERFACE "use: deriving(GQLType), will be automatically inferred" #-}

type INTERFACE = 'TYPE

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
