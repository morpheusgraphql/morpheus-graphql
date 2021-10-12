{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.NamedResolvers.Entities
  ( pagesApp,
  )
where

import Data.Morpheus (deriveApp)
import Data.Morpheus.NamedResolvers
  ( NamedResolverT,
    ResolveNamed (..),
    resolve,
  )
import Data.Morpheus.Types
  ( App,
    Arg (..),
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

instance Monad m => ResolveNamed m (Entity (NamedResolverT m)) where
  type Dep (Entity (NamedResolverT m)) = ID
  resolveNamed "zeus" = pure $ EntityDeity (resolve $ pure "zeus")
  resolveNamed "morpheus" = pure $ EntityDeity (resolve $ pure "morpheus")
  resolveNamed x = pure $ EntityRealm (resolve $ pure x)

-- QUERY
data Query m = Query
  { entities :: m [Entity m],
    entity :: Arg "id" ID -> m (Maybe (Entity m))
  }
  deriving
    ( Generic,
      GQLType
    )

instance Monad m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed () =
    pure
      Query
        { entities =
            resolve
              ( pure
                  [ "zeus",
                    "morpheus",
                    "olympus",
                    "dreams"
                  ]
              ),
          entity = \(Arg uid) -> resolve (pure (Just uid))
        }

pagesApp :: App () IO
pagesApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
