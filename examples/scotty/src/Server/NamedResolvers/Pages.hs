{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.NamedResolvers.Pages
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
    GQLError,
    GQLType (..),
    ID,
    MonadError,
    NamedResolvers (..),
    Undefined,
  )
import GHC.Generics (Generic)
import Server.NamedResolvers.Authors (Author, Post)

-- AUTHOR
data Page m
  = PagePost {unPost :: m (Post m)}
  | PageAuthor {unAuthor :: m (Author m)}
  deriving
    ( Generic,
      GQLType
    )

getPage :: (Monad m) => ID -> m (Page (NamedResolverT m))
getPage "1325" = pure $ PagePost (resolve $ pure "1325")
getPage "2525" = pure $ PagePost (resolve $ pure "2525")
getPage x = pure $ PageAuthor (resolve $ pure x)

instance ResolveNamed m (Page (NamedResolverT m)) where
  type Dep (Page (NamedResolverT m)) = ID
  resolveBatched = traverse (fmap Just . getPage)

-- QUERY
data Query m = Query
  { pages :: m [Page m],
    pageById :: Arg "id" ID -> m (Maybe (Page m))
  }
  deriving
    ( Generic,
      GQLType
    )

instance (MonadError GQLError m) => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveBatched _ =
    pure
      [ Just
          Query
            { pages = resolve (pure ["1325", "2415"]),
              pageById = \(Arg uid) -> resolve (pure uid)
            }
      ]

pagesApp :: App () IO
pagesApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
