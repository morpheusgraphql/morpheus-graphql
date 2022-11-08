{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Feature.NamedResolvers.EntitiesApp
  ( entitiesApp,
  )
where

import Control.Monad.Except
import Data.Morpheus.Server (deriveApp)
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
    GQLError,
    ID,
    Undefined,
  )
import Feature.NamedResolvers.DB
  ( allDeities,
    allEntities,
  )
import Feature.NamedResolvers.DeitiesApp ()
import Feature.NamedResolvers.Entities (Entity (..), Query (..))
import Feature.NamedResolvers.RealmsApp ()

getEntity :: (MonadError GQLError m) => ID -> m (Entity (NamedResolverT m))
getEntity name | name `elem` allDeities = pure $ EntityDeity $ resolve $ pure name
getEntity x = pure $ EntityRealm $ resolve $ pure x

instance ResolveNamed m (Entity (NamedResolverT m)) where
  type Dep (Entity (NamedResolverT m)) = ID
  resolveBatched = ignoreBatching getEntity

instance ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveBatched =
    ignoreBatching $
      const $
        pure
          Query
            { entities = resolve (pure allEntities),
              entity = \(Arg uid) -> resolve (pure uid)
            }

entitiesApp :: App () IO
entitiesApp = deriveApp (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
