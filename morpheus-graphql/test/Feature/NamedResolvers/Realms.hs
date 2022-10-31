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
import Data.Morpheus.Types
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
  resolveNamed = traverse getRealm
    where
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

instance Monad m => ResolveNamed m (Deity (NamedResolverT m)) where
  type Dep (Deity (NamedResolverT m)) = ID
  resolveNamed = traverse getDeity

getDeity :: (Monad m, Applicative f) => ID -> f (Deity (NamedResolverT m))
getDeity "zeus" =
  pure
    Deity
      { realm = resolve (pure "olympus")
      }
getDeity "morpheus" =
  pure
    Deity
      { realm = resolve (pure "dreams")
      }
getDeity x =
  pure
    Deity
      { realm = resolve (pure x)
      }

instance Monad m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed _ =
    pure
      [ Query
          { realm = \(Arg arg) -> resolve (pure (Just arg)),
            realms = resolve (pure ["olympus", "dreams"])
          }
      ]

realmsApp :: App () IO
realmsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
