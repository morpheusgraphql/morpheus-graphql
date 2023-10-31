{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.MonadIO.API where

import Control.Concurrent.STM
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Tuple.Extra (both)
import Network.HTTP.Types (Status (..))
import Web.Scotty
import Prelude hiding (id)

importGQLDocument "src/Server/MonadIO/schema.graphql"

-------------------------------------------------------------------------------
data DogRow = DogRow
  { dogId :: Int,
    dogName :: Text,
    ownerId :: Int
  }

type DogTable = [DogRow]

data UserRow = UserRow
  { userId :: Int,
    userUsername :: Text,
    userPassword :: Text,
    userFullName :: Text
  }

type UserTable = [UserRow]

data FollowRow = FollowRow
  { followerId :: Int,
    followeeId :: Int
  }

type FollowTable = [FollowRow]

-------------------------------------------------------------------------------
data Database = Database
  { userTable :: UserTable,
    dogTable :: DogTable,
    followTable :: FollowTable
  }

dbInit :: Database
dbInit =
  Database
    { dogTable =
        [ DogRow 1 "Golden Retriever" 1, --
          DogRow 2 "Labrador" 3,
          DogRow 2 "Chihu" 1
        ],
      userTable =
        [ UserRow 1 "user1" "123" "Jon Snow",
          UserRow 2 "user2" "123" "Night King",
          UserRow 3 "user3" "123" "Arya Stark"
        ],
      followTable =
        [ FollowRow 1 2, --
          FollowRow 1 3
        ]
    }

tokenUser1 :: Text
tokenUser1 = "aGVsbG8gd29ybGQKZGFzZmFzZGZhZGY="

tokenUser2 :: Text
tokenUser2 = "ZaHsJld3fJhZGZkYXN2m239ZGFzZg=="

tokenUser3 :: Text
tokenUser3 = "ZGtzamZkbGFrc2pmZmRhZmVy223="

-------------------------------------------------------------------------------
type ErrorCode = Int

type Headers = [(Text, Text)] -- Could be anything

data Env = Env
  { database :: TVar Database,
    reqHeaders :: Headers
  }

newtype Web a = Web
  { runWeb :: ReaderT Env (ExceptT ErrorCode IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Int,
      MonadIO
    )

-------------------------------------------------------------------------------

type RESOLVER t = (MonadTrans t, MonadIOResolver (t Web))

type RESTRICT o t = (MonadOperation (t Web) ~ o)

-- | Resolve value
type Value t (a :: k) = Flexible (t Web) a

-- | Resolve (f value)
type Wrapped t f (a :: k) = Composed (t Web) f a

-------------------------------------------------------------------------------
getDB :: (RESOLVER t) => Value t Database
getDB = do
  dbTVar <- lift $ asks database
  liftIO (readTVarIO dbTVar)

-------------------------------------------------------------------------------
requireAuthorized :: (RESOLVER t) => Value t Int
requireAuthorized = do
  reqHeaders <- lift $ asks reqHeaders
  case find ((== "Authorization") . fst) reqHeaders of
    Just (_, token)
      | token == tokenUser1 -> return 1
      | token == tokenUser2 -> return 2
      | token == tokenUser2 -> return 3
    _ -> fail "Unauthorized"

-------------------------------------------------------------------------------
rootResolver :: RootResolver Web () Query Mutation Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = resolveQuery,
      mutationResolver = resolveMutation
    }

resolveMutation :: (RESOLVER t, RESTRICT MUTATION t) => Mutation (t Web)
resolveMutation = Mutation {addDog = addDogResolver}

resolveQuery :: (RESOLVER t, RESTRICT QUERY t) => Query (t Web)
resolveQuery =
  Query
    { login = loginResolver,
      getUser = getUserResolver,
      dogs = dogsResolver
    }

-------------------------------------------------------------------------------
loginResolver :: (RESOLVER t, RESTRICT QUERY t) => LoginArgs -> Wrapped t Maybe Session
loginResolver LoginArgs {username, password} = do
  users <- fmap userTable getDB
  let match user =
        userUsername user == username && userPassword user == password
  case find match users of
    Just userRow -> do
      let tokenUser
            | userId userRow == 1 = tokenUser1
            | userId userRow == 2 = tokenUser2
            | otherwise = tokenUser3
      pure $
        Just $
          Session {token = pure tokenUser, user = userResolver userRow}
    Nothing -> fail "Invalid user or password"

getUserResolver :: (RESOLVER t) => Arg "id" Int -> Wrapped t Maybe User
getUserResolver (Arg argId) = do
  _ <- requireAuthorized
  users <- fmap userTable getDB
  case find ((== argId) . userId) users of
    Just userRow -> do
      user <- userResolver userRow
      pure $ Just user
    _ -> pure Nothing

dogsResolver :: (RESOLVER t) => Wrapped t [] Dog
dogsResolver = do
  _ <- requireAuthorized
  dogs <- fmap dogTable getDB
  traverse dogResolver dogs

-------------------------------------------------------------------------------
addDogResolver :: (RESOLVER t, RESTRICT MUTATION t) => Arg "name" Text -> Value t Dog
addDogResolver (Arg name) = do
  currentUserId <- requireAuthorized
  db <- getDB
  let dogs = dogTable db
      nxtId = (+ 1) . maximum . fmap dogId $ dogs
      dogToAdd = DogRow nxtId name currentUserId
      newDogs = dogToAdd : dogs
      newDb = db {dogTable = newDogs}
  dbTVar <- lift $ asks database
  liftIO $ atomically $ writeTVar dbTVar newDb
  dogResolver dogToAdd

-------------------------------------------------------------------------------
userResolver :: (RESOLVER t) => UserRow -> Value t User
userResolver UserRow {userId = thisUserId, userFullName} =
  pure $
    User
      { id = idResolver,
        name = nameResolver,
        favoriteDog = favoriteDogResolver,
        follows = followsResolver
      }
  where
    idResolver = pure thisUserId
    nameResolver = pure userFullName
    favoriteDogResolver :: (RESOLVER t) => Wrapped t Maybe Dog
    favoriteDogResolver = do
      dogs <- fmap dogTable getDB
      -- the 1st dog is the favorite dog
      case find ((== thisUserId) . ownerId) dogs of
        Just dogRow -> do
          dog <- dogResolver dogRow
          return . Just $ dog
        Nothing -> return Nothing
    followsResolver :: (RESOLVER t) => Wrapped t [] User
    followsResolver = do
      follows <- fmap followTable getDB
      users <- fmap userTable getDB
      let userFolloweeIds =
            map followeeId . filter ((== thisUserId) . followerId) $ follows
      let userFollowees = filter ((`elem` userFolloweeIds) . userId) users
      traverse userResolver userFollowees

dogResolver :: (RESOLVER t) => DogRow -> Value t Dog
dogResolver (DogRow dogId dogName ownerId) =
  pure $ Dog {id = idResolver, name = nameResolver, owner = ownerResolver}
  where
    idResolver = pure dogId
    nameResolver = pure dogName
    ownerResolver :: (RESOLVER t) => Value t User
    ownerResolver = do
      users <- fmap userTable getDB
      let userRow = fromJust . find ((== ownerId) . userId) $ users
      userResolver userRow

-------------------------------------------------------------------------------
api :: B.ByteString -> Web B.ByteString
api = interpreter rootResolver

app :: IO ()
app = do
  db <- newTVarIO dbInit
  scotty 8080 $
    post "/api" $
      do
        reqBody <- body
        reqHeaders <- headers
        let env = Env db $ map (both $ T.pack . LT.unpack) reqHeaders
        res <-
          liftIO . runExceptT . flip runReaderT env . runWeb $ api reqBody
        case res of
          Left code -> status $ Status code "Error"
          Right rawResponse -> raw rawResponse
