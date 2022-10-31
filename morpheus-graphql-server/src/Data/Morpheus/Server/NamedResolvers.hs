{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.NamedResolvers
  ( ResolveNamed (..),
    NamedResolverT (..),
    resolve,
  )
where

import Data.Aeson (ToJSON)
import Data.Morpheus.Types.ID (ID)
import Relude

instance Monad m => ResolveNamed m ID where
  type Dep ID = ID
  resolveNamed = pure

instance Monad m => ResolveNamed m Text where
  type Dep Text = Text
  resolveNamed = pure

class (ToJSON (Dep a)) => ResolveNamed (m :: Type -> Type) (a :: Type) where
  type Dep a :: Type
  resolveNamed :: Monad m => [Dep a] -> m [a]

instance (ResolveNamed m a) => ResolveNamed (m :: Type -> Type) [a] where
  type Dep [a] = [Dep a]
  resolveNamed = traverse resolveNamed

data NamedResolverT (m :: Type -> Type) a where
  Ref :: ResolveNamed m a => m (Dep a) -> NamedResolverT m a
  Refs :: ResolveNamed m a => m [Dep a] -> NamedResolverT m [a]
  Value :: m a -> NamedResolverT m a

-- RESOLVER TYPES
data RES = VALUE | LIST | REF

type family RES_TYPE a b :: RES where
  RES_TYPE a a = 'VALUE
  RES_TYPE [a] [b] = 'LIST
  RES_TYPE a b = 'REF

resolve :: forall m a b. (ResolveByType (RES_TYPE a b) m a b) => Monad m => m a -> NamedResolverT m b
resolve = resolveByType (Proxy :: Proxy (RES_TYPE a b))

class Dep b ~ a => ResolveByType (k :: RES) m a b where
  resolveByType :: Monad m => f k -> m a -> NamedResolverT m b

instance (ResolveNamed m a, Dep a ~ a) => ResolveByType 'VALUE m a a where
  resolveByType _ = Value

instance (ResolveNamed m b, Dep b ~ a) => ResolveByType 'LIST m [a] [b] where
  resolveByType _ = Refs

instance (ResolveNamed m b, Dep b ~ a) => ResolveByType 'REF m a b where
  resolveByType _ = Ref
