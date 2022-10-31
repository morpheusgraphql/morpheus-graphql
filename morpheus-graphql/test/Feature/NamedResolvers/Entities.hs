{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.NamedResolvers.Entities
  ( entitiesApp,
  )
where

import Control.Monad.Except
import Data.Morpheus (deriveApp)
import Data.Morpheus.NamedResolvers
  ( NamedResolverT,
    ResolveNamed (..),
    resolve,
  )
import Data.Morpheus.Types
  ( App,
    Arg (..),
    GQLError,
    GQLType (..),
    ID,
    NamedResolvers (..),
    Undefined,
  )
import Feature.NamedResolvers.Realms (Deity, Realm)
import GHC.Generics (Generic)

-- Entity
data Entity m
  = EntityDeity (m (Deity m))
  | EntityRealm (m (Realm m))
  deriving
    ( Generic,
      GQLType
    )

getEntity :: (Monad m, Applicative f) => ID -> f (Entity (NamedResolverT m))
getEntity "zeus" = pure $ EntityDeity (resolve $ pure "zeus")
getEntity "morpheus" = pure $ EntityDeity (resolve $ pure "morpheus")
getEntity x = pure $ EntityRealm (resolve $ pure x)

instance Monad m => ResolveNamed m (Entity (NamedResolverT m)) where
  type Dep (Entity (NamedResolverT m)) = ID
  resolveNamed = getEntity

-- QUERY
data Query m = Query
  { entities :: m [Entity m],
    entity :: Arg "id" ID -> m (Maybe (Entity m))
  }
  deriving
    ( Generic,
      GQLType
    )

instance MonadError GQLError m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed _ =
    pure
      Query
        { entities = resolve (pure ["zeus", "morpheus", "olympus", "dreams"]),
          entity = \(Arg uid) -> resolve (pure (Just uid))
        }

entitiesApp :: App () IO
entitiesApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
