{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Domains.Users.Resolver where

import Data.Data
import Data.Maybe (catMaybes)
import Data.Morpheus (deriveApp)
import Data.Morpheus.Types
import Domains.Users.Users

resolveUser ::
  (Monad m) =>
  ID ->
  m (Maybe (User m))
resolveUser postId =
  pure
    $ Just
    $ User
      { Domains.Users.Users.id = pure postId,
        name = pure "User Tittle"
      }

resolveQuery :: (Monad m) => Query m
resolveQuery =
  Query
    { users =
        catMaybes
          <$> traverse
            resolveUser
            [ "id1",
              "id2"
            ],
      user = resolveUser . argValue
    }

rootResolver ::
  (Monad m) =>
  RootResolver m () Query Undefined Undefined
rootResolver = defaultRootResolver {queryResolver = resolveQuery}

app :: (Typeable m, Monad m) => App () m
app = deriveApp rootResolver
