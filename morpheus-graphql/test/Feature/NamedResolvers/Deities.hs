{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Feature.NamedResolvers.Deities
  ( deitiesApp,
  )
where

import Data.Morpheus (deriveApp)
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
    NamedResolvers (..),
    Undefined,
  )
import Relude hiding (Undefined)

importGQLDocument "test/Feature/NamedResolvers/deities.gql"

getPower :: (Eq a, IsString a, Applicative f) => a -> f Power
getPower "sp" = pure Shapeshifting
getPower _ = pure Thunderbolt

getDeity :: (Eq a, Monad m, Applicative f, IsString a) => a -> f (Deity (NamedResolverT m))
getDeity "zeus" =
  pure
    Deity
      { name = resolve (pure "Zeus"),
        power = resolve (pure ["tb"])
      }
getDeity "morpheus" =
  pure
    Deity
      { name = resolve (pure "Morpheus"),
        power = resolve (pure ["sp"])
      }
getDeity _ =
  pure
    Deity
      { name = resolve (pure "Unknown"),
        power = resolve (pure [])
      }

instance Monad m => ResolveNamed m Power where
  type Dep Power = ID
  resolveNamed = traverse getPower

instance Monad m => ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveNamed = traverse getDeity

instance Monad m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed _ =
    pure
      [ Query
          { deity = \(Arg uid) -> resolve (pure (Just uid)),
            deities = resolve (pure ["zeus", "morpheus"])
          }
      ]

deitiesApp :: App () IO
deitiesApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
