{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Users.Resolver where

import API.Users.Users
import Data.Data
import Data.Maybe (catMaybes)
import Data.Morpheus (deriveApp)
import Data.Morpheus.Types

resolveUser ::
  Monad m =>
  ID ->
  m (Maybe (User m))
resolveUser postId =
  pure $
    Just $
      User
        { API.Users.Users.id = pure postId,
          name = pure "User Tittle"
        }

resolveQuery :: Monad m => Query m
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
  Monad m =>
  RootResolver m () Query Undefined Undefined
rootResolver = defaultRootResolver {queryResolver = resolveQuery}

app :: (Typeable m, Monad m) => App () m
app = deriveApp rootResolver
