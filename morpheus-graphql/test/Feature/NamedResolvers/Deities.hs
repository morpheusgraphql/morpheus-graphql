{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Except
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
    GQLError,
    ID,
    NamedResolvers (..),
    Undefined,
  )
import Relude hiding (Undefined)

importGQLDocument "test/Feature/NamedResolvers/deities.gql"

getPower :: (Eq a, IsString a, Applicative f) => a -> f Power
getPower "sp" = pure Shapeshifting
getPower _ = pure Thunderbolt

getDeity :: (Eq a, Monad m, Applicative f, IsString a) => a -> f (Maybe (Deity (NamedResolverT m)))
getDeity "zeus" =
  pure $
    Just
      Deity
        { name = resolve (pure "Zeus"),
          power = resolve (pure ["tb"])
        }
getDeity "morpheus" =
  pure $
    Just
      Deity
        { name = resolve (pure "Morpheus"),
          power = resolve (pure ["sp"])
        }
getDeity _ = pure Nothing

batched :: (Traversable t, Applicative f) => (a1 -> f a2) -> t a1 -> f (t (Maybe a2))
batched f = traverse (fmap Just . f)

instance Monad m => ResolveNamed m Power where
  type Dep Power = ID
  resolveNamed = batched getPower

instance Monad m => ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveNamed = traverse getDeity

instance MonadError GQLError m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed _ =
    pure
      [ Just $
          Query
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
