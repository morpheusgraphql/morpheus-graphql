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
  )
where

import Relude

data GQL_KIND
  = SCALAR
  | ENUM
  | AUTO
  | WRAPPER
  | INTERFACE

class ToValue (a :: GQL_KIND) where
  toValue :: f a -> GQL_KIND

instance ToValue 'SCALAR where
  toValue _ = SCALAR

instance ToValue 'ENUM where
  toValue _ = ENUM

instance ToValue 'WRAPPER where
  toValue _ = WRAPPER

instance ToValue 'AUTO where
  toValue _ = AUTO

instance ToValue 'INTERFACE where
  toValue _ = INTERFACE

isObject :: GQL_KIND -> Bool
isObject AUTO = True
isObject INTERFACE = True
isObject _ = False

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
type SCALAR = 'SCALAR

-- | GraphQL Enum
type ENUM = 'ENUM

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'WRAPPER

{-# DEPRECATED OUTPUT "use: deriving(GQLType), will be automatically inferred" #-}

-- | GraphQL Object and union
type OUTPUT = 'AUTO

{-# DEPRECATED INPUT "use: deriving(GQLType), will be automatically inferred" #-}

-- | GraphQL input Object and input union
type INPUT = 'AUTO

{-# DEPRECATED INPUT_OBJECT "use: deriving(GQLType), will be automatically inferred" #-}

-- | GraphQL input Object
type INPUT_OBJECT = 'AUTO

{-# DEPRECATED UNION "use: deriving(GQLType), IMPORTANT: only types with <type constructor name><constructor name> will sustain their form, other union constructors will be wrapped inside an new object" #-}

-- | GraphQL Union
type UNION = 'AUTO

{-# DEPRECATED OBJECT "use: deriving(GQLType), will be automatically inferred" #-}

-- | GraphQL Object
type OBJECT = 'AUTO

type INTERFACE = 'INTERFACE
