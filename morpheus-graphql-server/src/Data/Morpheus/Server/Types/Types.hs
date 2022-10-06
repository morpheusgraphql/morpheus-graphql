{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Types
  ( Undefined (..),
    Pair (..),
    TypeGuard (..),
    Arg (..),
    __typenameUndefined,
  )
where

import Data.Kind (Type)
import Data.Morpheus.Types.Internal.AST (TypeName)
import GHC.Generics
  ( Generic,
  )
import GHC.TypeLits (Symbol)
import Prelude (Bool, Show)

__typenameUndefined :: TypeName
__typenameUndefined = "Undefined"

newtype Undefined (m :: Type -> Type) = Undefined Bool deriving (Show, Generic)

data Pair k v = Pair k v deriving (Generic)

data TypeGuard interface union
  = ResolveInterface interface
  | ResolveType union

newtype Arg (name :: Symbol) a = Arg {argValue :: a}
  deriving
    ( Show,
      Generic
    )
