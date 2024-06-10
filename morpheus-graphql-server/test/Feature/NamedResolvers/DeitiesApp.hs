{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Feature.NamedResolvers.DeitiesApp
  ( deitiesApp,
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
    ID,
    Undefined,
  )
import Feature.NamedResolvers.DB (allDeities, getDeityName, getPowers)
import Feature.NamedResolvers.Deities
import Relude hiding (Undefined)

getPower :: (Monad m) => ID -> m (Maybe Power)
getPower "sp" = pure (Just Shapeshifting)
getPower "tb" = pure (Just Thunderbolt)
getPower _ = pure Nothing

getDeity :: (Monad m) => ID -> m (Maybe (Deity (NamedResolverT m)))
getDeity uid
  | uid `elem` allDeities =
      pure
        $ Just
          Deity
            { name = resolve (getDeityName uid),
              power = resolve (getPowers uid),
              description = resolve (pure uid)
            }
getDeity _ = pure Nothing

instance ResolveNamed m Power where
  type Dep Power = ID
  resolveBatched = traverse getPower

instance ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveBatched = traverse getDeity

instance ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveBatched =
    ignoreBatching
      $ const
      $ pure
        Query
          { deity = \(Arg uid) -> resolve (pure uid),
            deities = resolve (pure allDeities)
          }

deitiesApp :: App () IO
deitiesApp = deriveApp (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
