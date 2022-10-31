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

instance Monad m => ResolveNamed m (Page (NamedResolverT m)) where
  type Dep (Page (NamedResolverT m)) = ID
  resolveNamed "1325" = pure $ PagePost (resolve $ pure "1325")
  resolveNamed "2525" = pure $ PagePost (resolve $ pure "2525")
  resolveNamed x = pure $ PageAuthor (resolve $ pure x)

-- QUERY
data Query m = Query
  { pages :: m [Page m],
    pageById :: Arg "id" ID -> m (Maybe (Page m))
  }
  deriving
    ( Generic,
      GQLType
    )

instance MonadError GQLError m => ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveNamed () =
    pure
      Query
        { pages = resolve (pure ["1325", "2415"]),
          pageById = \(Arg uid) -> resolve (pure (Just uid))
        }

pagesApp :: App () IO
pagesApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
