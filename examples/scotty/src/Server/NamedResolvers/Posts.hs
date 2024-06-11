{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server.NamedResolvers.Posts
  ( postsApp,
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
    ID (unpackID),
    MonadError,
    NamedResolvers (..),
    Undefined,
  )
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude
  ( IO,
    Maybe (..),
    Monad (..),
    pure,
    traverse,
    ($),
  )

-- POST
data Post m = Post
  { postID :: m ID,
    title :: m Text
  }
  deriving (Generic, GQLType)

getPost :: (Monad m) => ID -> m (Maybe (Post (NamedResolverT m)))
getPost pid =
  pure
    $ Just
    $ Post
      { postID = resolve (pure pid),
        title = resolve (pure $ "title for \"" <> unpackID pid <> "\"")
      }

instance ResolveNamed m (Post (NamedResolverT m)) where
  type Dep (Post (NamedResolverT m)) = ID
  resolveBatched = traverse getPost

-- QUERY
data Query m = Query
  { posts :: m [Post m],
    post :: Arg "id" ID -> m (Maybe (Post m))
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
            { posts = resolve (pure ["1325", "2525"]),
              post = \(Arg arg) -> resolve (pure arg)
            }
      ]

postsApp :: App () IO
postsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
