{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Types
  ( Undefined (..),
    Pair (..),
    Guard (..),
  )
where

import GHC.Generics
  ( Generic,
  )
import GHC.TypeLits (Symbol)
import Prelude
  ( Show,
  )

data Undefined (m :: * -> *) = Undefined deriving (Show, Generic)

data Pair k v = Pair
  { key :: k,
    value :: v
  }
  deriving (Generic)

newtype Guard (name :: Symbol) interface union
  = Guard union
