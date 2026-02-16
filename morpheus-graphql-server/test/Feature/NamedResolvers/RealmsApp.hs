{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Feature.NamedResolvers.RealmsApp
  ( realmsApp,
    Deity,
    Realm,
  )
where

import Data.Morpheus.Server
  ( deriveApp,
  )
import Data.Morpheus.Server.Resolvers
  ( NamedResolverT,
    NamedResolvers (..),
    ResolveNamed (..),
    ignoreBatching,
    resolve,
  )
import Data.Morpheus.Server.Types
  ( App,
    Arg (..),
    ID,
    Undefined,
  )
import Feature.NamedResolvers.DB
  ( allDeities,
    allRealms,
    getOwner,
    getPlace,
    getRealmName,
  )
import Feature.NamedResolvers.Realms

getRealm :: (Monad m) => ID -> m (Maybe (Realm (NamedResolverT m)))
getRealm uid
  | uid `elem` allRealms =
      pure $
        Just
          Realm
            { name = resolve (getRealmName uid),
              owner = resolve (getOwner uid),
              description = resolve (pure uid)
            }
getRealm _ = pure Nothing

instance ResolveNamed m (Realm (NamedResolverT m)) where
  type Dep (Realm (NamedResolverT m)) = ID

  resolveBatched = traverse getRealm

instance ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveBatched = traverse getDeity

getDeity :: (Monad m) => ID -> m (Maybe (Deity (NamedResolverT m)))
getDeity arg
  | arg `elem` allDeities = pure $ Just Deity {realm = resolve (getPlace arg)}
  | otherwise = pure Nothing

instance ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveBatched =
    ignoreBatching $
      const $
        pure
          Query
            { realm = \(Arg arg) -> resolve (pure arg),
              realms = resolve (pure allRealms)
            }

realmsApp :: App () IO
realmsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
