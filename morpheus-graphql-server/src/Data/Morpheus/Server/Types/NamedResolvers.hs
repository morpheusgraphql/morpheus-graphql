{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.NamedResolvers
  ( ResolveNamed (..),
    NamedResolverT (..),
    resolve,
    useBatched,
    Dependency,
    ignoreBatching,
    NamedRef,
  )
where

import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST (GQLError, internal)
import Data.Vector (Vector)
import Relude

type family Target a :: Type where
  Target (Maybe a) = a
  Target [a] = a
  Target (Set a) = a
  Target (NonEmpty a) = a
  Target (Seq a) = a
  Target (Vector a) = a
  Target a = a

type family Dependency a :: Type where
-- wrappers
  Dependency (Maybe a) = Dependency a
  Dependency [a] = Dependency a
  Dependency (Set a) = Dependency a
  Dependency (NonEmpty a) = Dependency a
  Dependency (Seq a) = Dependency a
  Dependency (Vector a) = Dependency a
-- custom
  Dependency a = Dep a

ignoreBatching :: (Monad m) => (a -> m b) -> [a] -> m [Maybe b]
ignoreBatching f = traverse (fmap Just . f)

forward :: (Monad m, Dependency a ~ a) => [Dependency a] -> m [Maybe a]
forward = pure . map Just

{-# DEPRECATED useBatched " this function is obsolete" #-}
useBatched :: (ResolveNamed m a, MonadError GQLError m) => Dependency a -> m a
useBatched x = resolveBatched [x] >>= res
  where
    res [Just v] = pure v
    res _ = throwError (internal "named resolver should return single value for single argument")

{-# DEPRECATED resolveNamed "use: resolveBatched" #-}

instance ResolveNamed m Int where
  type Dep Int = Int
  resolveBatched = forward

instance ResolveNamed m Float where
  type Dep Float = Float
  resolveBatched = forward

instance ResolveNamed m Double where
  type Dep Double = Double
  resolveBatched = forward

instance ResolveNamed m Text where
  type Dep Text = Text
  resolveBatched = forward

instance ResolveNamed m Bool where
  type Dep Bool = Bool
  resolveBatched = forward

instance ResolveNamed m ID where
  type Dep ID = ID
  resolveBatched = forward

class ToJSON (Dependency a) => ResolveNamed (m :: Type -> Type) (a :: Type) where
  type Dep a :: Type
  resolveBatched :: MonadError GQLError m => [Dependency a] -> m [Maybe a]

  resolveNamed :: MonadError GQLError m => Dependency a -> m a
  resolveNamed = useBatched

data NamedResolverT (m :: Type -> Type) a where
  NamedResolverT :: ResolveNamed m (Target a) => m (NamedRef a) -> NamedResolverT m a

type family NamedRef a :: Type where
  NamedRef [a] = [Dependency a]
  NamedRef (Set a) = [Dependency a]
  NamedRef (NonEmpty a) = [Dependency a]
  NamedRef (Seq a) = [Dependency a]
  NamedRef (Vector a) = [Dependency a]
  NamedRef a = Dependency a

resolve :: ResolveNamed m (Target a) => m (NamedRef a) -> NamedResolverT m a
resolve = NamedResolverT
