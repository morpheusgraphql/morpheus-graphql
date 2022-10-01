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
import Data.Morpheus.Server.Types
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

instance Monad m => ResolveNamed m Power where
  type Dep Power = ID
  resolveNamed "sp" = pure Shapeshifting
  resolveNamed _ = pure Thunderbolt

instance Monad m => ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveNamed "zeus" =
    pure
      Deity
        { name = resolve (pure "Zeus"),
          power = resolve (pure ["tb"])
        }
  resolveNamed "morpheus" =
    pure
      Deity
        { name = resolve (pure "Morpheus"),
          power = resolve (pure ["sp"])
        }
  resolveNamed _ =
    pure
      Deity
        { name = resolve (pure "Unknown"),
          power = resolve (pure [])
        }

instance Monad m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed () =
    pure
      Query
        { deity = \(Arg uid) -> resolve (pure (Just uid)),
          deities = resolve (pure ["zeus", "morpheus"])
        }

deitiesApp :: App () IO
deitiesApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
