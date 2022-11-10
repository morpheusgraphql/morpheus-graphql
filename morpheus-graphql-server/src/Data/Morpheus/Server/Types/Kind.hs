{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | associating types to GraphQL Kinds
module Data.Morpheus.Server.Types.Kind
  ( SCALAR,
    DerivingKind,
    TYPE,
    CUSTOM,
    WRAPPER,
  )
where

import Relude

data DerivingKind
  = DERIVING_SCALAR
  | DERIVING_TYPE
  | DERIVING_WRAPPER
  | DERIVING_CUSTOM
  deriving (Show)

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
type SCALAR = 'DERIVING_SCALAR

-- | GraphQL input, type, union , enum
type TYPE = 'DERIVING_TYPE

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'DERIVING_WRAPPER

type CUSTOM = 'DERIVING_CUSTOM
