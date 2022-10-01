{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | associating types to GraphQL Kinds
module Data.Morpheus.Server.Types.Kind
  ( SCALAR,
    DerivingKind (..),
    TYPE,
    CUSTOM,
    WRAPPER,
  )
where

import Relude

data DerivingKind
  = SCALAR
  | TYPE
  | WRAPPER
  | CUSTOM
  deriving (Show)

-- | GraphQL input, type, union , enum
type TYPE = 'TYPE

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
type SCALAR = 'SCALAR

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'WRAPPER

type CUSTOM = 'CUSTOM
