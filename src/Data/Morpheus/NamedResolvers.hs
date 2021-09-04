{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.NamedResolvers
  ( ResolveNamed (..),
    ref,
    maybeRef,
    refs,
    value,
    RefResolver,
  )
where

import Data.Morpheus.Server.Deriving.Decode
import Data.Morpheus.Server.Types.GQLType
import Relude

class (Decode (Dep res)) => ResolveNamed (m :: * -> *) (res :: (* -> *) -> *) where
  type Dep res :: *
  resolveNamed :: Monad m => Dep res -> m (res (RefResolver m))

data RefResolver (m :: * -> *) a where
  Ref :: (ResolveNamed m res => m (Dep res)) -> RefResolver m (res (RefResolver m))
  MaybeRef :: (ResolveNamed m res => m (Dep res)) -> RefResolver m (Maybe (res (RefResolver m)))
  Refs :: (ResolveNamed m res => m [Dep res]) -> RefResolver m [res (RefResolver m)]
  Val :: m res -> RefResolver m res

instance (GQLType a) => GQLType (RefResolver m a) where
  type KIND (RefResolver m a) = KIND a
  __type _ = __type (Proxy :: Proxy a)

value :: m res -> RefResolver m res
value = Val

ref :: m (Dep res) -> RefResolver m (res (RefResolver m))
ref = Ref

maybeRef :: m (Dep res) -> RefResolver m (Maybe (res (RefResolver m)))
maybeRef = MaybeRef

refs :: Monad m => ResolveNamed m res => m [Dep res] -> RefResolver m [res (RefResolver m)]
refs = Refs
