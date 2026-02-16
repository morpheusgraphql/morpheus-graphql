{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.Posts.Resolver where

import Data.Data (Typeable)
import Data.Maybe (catMaybes)
import Data.Morpheus (deriveApp)
import Data.Morpheus.Types
import Domains.Posts.Posts

resolvePost ::
  (Monad m) =>
  ID ->
  m (Maybe (Post m))
resolvePost postId =
  pure $
    Just $
      Post
        { Domains.Posts.Posts.id = pure postId,
          title = pure "Post Tittle",
          authorID = pure "Post Author"
        }

resolveQuery :: (Monad m) => Query m
resolveQuery =
  Query
    { posts =
        catMaybes
          <$> traverse
            resolvePost
            [ "id1",
              "id2"
            ],
      post = resolvePost . argValue
    }

rootResolver ::
  (Monad m) =>
  RootResolver m () Query Undefined Undefined
rootResolver = defaultRootResolver {queryResolver = resolveQuery}

app :: (Typeable m, Monad m) => App () m
app = deriveApp rootResolver
