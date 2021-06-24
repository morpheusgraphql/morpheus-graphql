{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Types
  ( Undefined (..),
    Pair (..),
    TypeGuard (..),
  )
where

import GHC.Generics
  ( Generic,
  )
import Prelude
  ( Show,
  )

data Undefined (m :: * -> *) = Undefined deriving (Show, Generic)

data Pair k v = Pair
  { key :: k,
    value :: v
  }
  deriving (Generic)

data TypeGuard interface union
  = ResolveInterface interface
  | ResolveType union
