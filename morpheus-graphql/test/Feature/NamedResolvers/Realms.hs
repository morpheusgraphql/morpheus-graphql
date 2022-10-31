{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies ,FlexibleContexts#-}

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
    ID,
    GQLError,
    NamedResolvers (..),
    Undefined,
  )
import Data.Text (Text)
import Control.Monad.Except

importGQLDocument "test/Feature/NamedResolvers/realms.gql"

getRealm "olympus" =
  pure
    Realm
      { name = resolve (pure "Mount Olympus"),
        owner = resolve (pure "zeus")
      }
getRealm "dreams" =
  pure
    Realm
      { name = resolve (pure "Fictional world of dreams"),
        owner = resolve (pure "morpheus")
      }
getRealm _ =
  pure
    Realm
      { name = resolve (pure "Unknown"),
        owner = resolve (pure "none")
      }

batched f = traverse (fmap Just . f)

instance Monad m => ResolveNamed m (Realm (NamedResolverT m)) where
  type Dep (Realm (NamedResolverT m)) = ID
  resolveNamed = batched getRealm

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
