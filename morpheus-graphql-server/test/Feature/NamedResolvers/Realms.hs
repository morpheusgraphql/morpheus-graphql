{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Data.Morpheus.Server.Types
  ( App,
    Arg (..),
    ID,
    NamedResolvers (..),
    Undefined,
  )
import Data.Text (Text)

importGQLDocument "test/Feature/NamedResolvers/realms.gql"

instance Monad m => ResolveNamed m (Realm (NamedResolverT m)) where
  type Dep (Realm (NamedResolverT m)) = ID
  resolveNamed "olympus" =
    pure
      Realm
        { name = resolve (pure "Mount Olympus"),
          owner = resolve (pure "zeus")
        }
  resolveNamed "dreams" =
    pure
      Realm
        { name = resolve (pure "Fictional world of dreams"),
          owner = resolve (pure "morpheus")
        }
  resolveNamed _ =
    pure
      Realm
        { name = resolve (pure "Unknown"),
          owner = resolve (pure "none")
        }

instance Monad m => ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveNamed "zeus" =
    pure
      Deity
        { realm = resolve (pure "olympus")
        }
  resolveNamed "morpheus" =
    pure
      Deity
        { realm = resolve (pure "dreams")
        }
  resolveNamed x =
    pure
      Deity
        { realm = resolve (pure x)
        }

instance Monad m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed () =
    pure
      Query
        { realm = \(Arg arg) -> resolve (pure (Just arg)),
          realms = resolve (pure ["olympus", "dreams"])
        }

realmsApp :: App () IO
realmsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
