{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.API where

import Data.Data (Typeable)
import Data.Morpheus (deriveApp)
import Data.Morpheus.Types
import Scalars (Markdown (..))
import Server.Blog

resolvePost ::
  (Monad m) =>
  ID ->
  m (Post m)
resolvePost (ID x) =
  pure
    Post
      { title = pure x,
        body = pure $ Just $ Markdown "body"
      }

resolveUser ::
  (Monad m) =>
  ID ->
  m (User m)
resolveUser (ID x) =
  pure
    User
      { name = pure x,
        posts = traverse resolvePost ["id1", "id2"]
      }

resolveQuery :: (Monad m) => Query m
resolveQuery =
  Query
    { getPosts = traverse resolvePost ["id1", "id2"],
      getUsers = traverse resolveUser ["id1", "id2"]
    }

rootResolver ::
  (Monad m) =>
  RootResolver m () Query Undefined Undefined
rootResolver = defaultRootResolver {queryResolver = resolveQuery}

app :: (Typeable m, Monad m) => App () m
app = deriveApp rootResolver
