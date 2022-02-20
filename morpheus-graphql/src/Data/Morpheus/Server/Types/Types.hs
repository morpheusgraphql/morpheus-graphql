{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Types
  ( Undefined (..),
    Pair (..),
    TypeGuard (..),
    Arg (..),
  )
where

import Data.Kind (Type)
import GHC.Generics
  ( Generic,
  )
import GHC.TypeLits (Symbol)
import Prelude
  ( Show,
  )

data Undefined (m :: Type -> Type) = Undefined deriving (Show, Generic)

data Pair k v = Pair
  { key :: k,
    value :: v
  }
  deriving (Generic)

data TypeGuard interface union
  = ResolveInterface interface
  | ResolveType union

newtype Arg (name :: Symbol) a = Arg {argValue :: a}
  deriving
    ( Show,
      Generic
    )
