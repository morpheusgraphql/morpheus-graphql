{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Resolvers
  ( ResolveNamed (..),
    NamedResolverT (..),
    resolve,
    NamedResolvers (..),
    RootResolver (..),
    defaultRootResolver,
  )
where

import Data.Morpheus.App.Internal.Resolving (Resolver)
import Data.Morpheus.Server.NamedResolvers
import Data.Morpheus.Server.Types.Types
  ( Undefined (..),
  )
import Data.Morpheus.Types.Internal.AST
import Relude hiding (Undefined)

data
  NamedResolvers
    (m :: Type -> Type)
    event
    (qu :: (Type -> Type) -> Type)
    (mu :: (Type -> Type) -> Type)
    (su :: (Type -> Type) -> Type)
  = ( ResolveNamed
        (Resolver QUERY event m)
        (qu (NamedResolverT (Resolver QUERY event m)))
    ) =>
    NamedResolvers

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you can use __()__ for it.
data
  RootResolver
    (m :: Type -> Type)
    event
    (query :: (Type -> Type) -> Type)
    (mutation :: (Type -> Type) -> Type)
    (subscription :: (Type -> Type) -> Type) = RootResolver
  { queryResolver :: query (Resolver QUERY event m),
    mutationResolver :: mutation (Resolver MUTATION event m),
    subscriptionResolver :: subscription (Resolver SUBSCRIPTION event m)
  }

defaultRootResolver :: RootResolver m event Undefined Undefined Undefined
defaultRootResolver =
  RootResolver
    { queryResolver = Undefined False,
      mutationResolver = Undefined False,
      subscriptionResolver = Undefined False
    }
