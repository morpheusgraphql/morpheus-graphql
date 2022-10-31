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

batched :: (Traversable t, Applicative f) => (a1 -> f a2) -> t a1 -> f (t (Maybe a2))
batched f = traverse (fmap Just . f)

instance Monad m => ResolveNamed m (Realm (NamedResolverT m)) where
  type Dep (Realm (NamedResolverT m)) = ID
  resolveNamed = traverse getRealm

instance Monad m => ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveNamed = batched getDeity

getDeity :: (Monad m, Applicative f) => ID -> f (Deity (NamedResolverT m))
getDeity "zeus" = pure Deity {realm = resolve (pure "olympus")}
getDeity "morpheus" = pure Deity {realm = resolve (pure "dreams")}
getDeity x = pure Deity {realm = resolve (pure x)}

instance MonadError GQLError m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed _ =
    pure
      [ Just $
          Query
            { realm = \(Arg arg) -> resolve (pure (Just arg)),
              realms = resolve (pure ["olympus", "dreams"])
            }
      ]

realmsApp :: App () IO
realmsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
