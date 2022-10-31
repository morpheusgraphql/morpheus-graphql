{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.NamedResolvers.Realms
  ( realmsApp,
    Deity,
    Realm,
  )
where

import Control.Monad.Except
import Data.Morpheus
  ( deriveApp,
  )
import Data.Morpheus.Document
  ( importGQLDocument,
  )
import Data.Morpheus.NamedResolvers
  ( NamedResolverT,
    ResolveNamed (..),
    resolve,
  )
import Data.Morpheus.Types
  ( App,
    Arg (..),
    GQLError,
    ID,
    NamedResolvers (..),
    Undefined,
  )
import Data.Text (Text)

importGQLDocument "test/Feature/NamedResolvers/realms.gql"

getRealm :: (Monad m) => ID -> m (Maybe (Realm (NamedResolverT m)))
getRealm "olympus" =
  pure $
    Just
      Realm
        { name = resolve (pure "Mount Olympus"),
          owner = resolve (pure "zeus")
        }
getRealm "dreams" =
  pure $
    Just
      Realm
        { name = resolve (pure "Fictional world of dreams"),
          owner = resolve (pure "morpheus")
        }
getRealm _ = pure Nothing

instance Monad m => ResolveNamed m (Realm (NamedResolverT m)) where
  type Dep (Realm (NamedResolverT m)) = ID
  resolveBatched = traverse getRealm

instance Monad m => ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveBatched = traverse getDeity

getDeity :: (Monad m, Applicative f) => ID -> f (Maybe (Deity (NamedResolverT m)))
getDeity "zeus" = pure $ Just Deity {realm = resolve (pure "olympus")}
getDeity "morpheus" = pure $ Just Deity {realm = resolve (pure "dreams")}
getDeity _ = pure Nothing

instance MonadError GQLError m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed _ =
    pure
      Query
        { realm = \(Arg arg) -> resolve (pure (Just arg)),
          realms = resolve (pure ["olympus", "dreams"])
        }

realmsApp :: App () IO
realmsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
