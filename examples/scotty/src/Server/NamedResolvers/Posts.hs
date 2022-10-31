{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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

-- POST
data Post m = Post
  { postID :: m ID,
    title :: m Text
  }
  deriving (Generic, GQLType)

instance Monad m => ResolveNamed m (Post (NamedResolverT m)) where
  type Dep (Post (NamedResolverT m)) = ID
  resolveNamed pid =
    pure
      Post
        { postID = resolve (pure pid),
          title = resolve (pure $ "title for \"" <> unpackID pid <> "\"")
        }

-- QUERY
data Query m = Query
  { posts :: m [Post m],
    post :: Arg "id" ID -> m (Maybe (Post m))
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
        { posts = resolve (pure ["1325", "2525"]),
          post = \(Arg arg) -> resolve (pure (Just arg))
        }

postsApp :: App () IO
postsApp =
  deriveApp
    (NamedResolvers :: NamedResolvers IO () Query Undefined Undefined)
