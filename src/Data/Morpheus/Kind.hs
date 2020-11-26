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
  | INPUT
  | OUTPUT
  | WRAPPER
  | INTERFACE
  | AUTO

class ToValue (a :: GQL_KIND) where
  toValue :: f a -> GQL_KIND

instance ToValue 'SCALAR where
  toValue _ = SCALAR

instance ToValue 'ENUM where
  toValue _ = ENUM

instance ToValue 'WRAPPER where
  toValue _ = WRAPPER

instance ToValue 'INPUT where
  toValue _ = INPUT

instance ToValue 'OUTPUT where
  toValue _ = OUTPUT

instance ToValue 'INTERFACE where
  toValue _ = INTERFACE

isObject :: GQL_KIND -> Bool
isObject INPUT = True
isObject OUTPUT = True
isObject INTERFACE = True
isObject _ = False

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
type SCALAR = 'SCALAR

-- | GraphQL Enum
type ENUM = 'ENUM

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'WRAPPER

{-# DEPRECATED OUTPUT "use more generalized kind: AUTO" #-}

-- | GraphQL Object and union
type OUTPUT = 'OUTPUT

{-# DEPRECATED INPUT "use more generalized kind: AUTO" #-}

-- | GraphQL input Object and input union
type INPUT = 'INPUT

{-# DEPRECATED INPUT_OBJECT "use more generalized kind: AUTO" #-}

-- | GraphQL input Object
type INPUT_OBJECT = 'INPUT

{-# DEPRECATED UNION "use: deriving(GQLType), IMPORTANT: only types with <type constructor name><constructor name> will sustain their form, other union constructors will be wrapped inside an new object" #-}

-- | GraphQL Union
type UNION = 'OUTPUT

{-# DEPRECATED OBJECT "use: deriving(GQLType), will be automatically inferred" #-}

-- | GraphQL Object
type OBJECT = 'OUTPUT

type INTERFACE = 'INTERFACE
