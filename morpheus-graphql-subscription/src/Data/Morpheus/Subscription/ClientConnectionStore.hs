{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Subscription.ClientConnectionStore
  ( SessionID (..),
    ClientConnectionStore,
    ClientConnection,
    Updates (..),
    startSession,
    endSession,
    empty,
    insert,
    delete,
    publish,
    toList,
    connectionSessionIds,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    KeyOf (..),
  )
import Data.Morpheus.Subscription.Apollo
  ( toApolloResponse,
  )
import Data.Morpheus.Subscription.Event (Event (..))
import Data.Morpheus.Types.IO (GQLResponse)
import Data.Morpheus.Types.Internal.Resolving
  ( EventHandler (..),
  )
import Data.UUID (UUID)
import Relude hiding
  ( ByteString,
    Show,
    empty,
    show,
    toList,
  )
import Prelude (Show (..))

data SessionID = SessionID
  { cid :: UUID,
    sid :: Text
  }
  deriving (Show, Generic, Eq, Hashable)

data ClientConnection (m :: * -> *) = ClientConnection
  { connectionId :: UUID,
    connectionCallback :: ByteString -> m (),
    -- one connection can have multiple subscription session
    connectionSessionIds :: [Text]
  }

data ClientSession e (m :: * -> *) = ClientSession
  { sessionChannel :: Channel e,
    sessionCallback :: e -> m ()
  }

instance Show (ClientSession e m) where
  show ClientSession {} = "ClientSession"

instance Show (ClientConnection m) where
  show ClientConnection {connectionId, connectionSessionIds} =
    "ClientConnection { id = "
      <> show connectionId
      <> ", sessions = "
      <> show connectionSessionIds
      <> " }"

mapAt :: (Eq k, Hashable k) => c -> (a -> c) -> k -> HashMap k a -> c
mapAt fallback f key = maybe fallback f . HM.lookup key

publish ::
  ( Monad m,
    Eq channel
  ) =>
  Event channel content ->
  ClientConnectionStore (Event channel content) m ->
  m ()
publish event ClientConnectionStore {activeChannels, clientSessions} =
  traverse_ sendByChannel activeChannels
  where
    sendByChannel sid = mapAt (pure ()) sendMessage sid clientSessions
    sendMessage ClientSession {sessionChannel, sessionCallback}
      | sessionChannel `elem` getChannels event = sessionCallback event
      | otherwise = pure ()

newtype Updates e (m :: * -> *) = Updates
  { _runUpdate :: ClientConnectionStore e m -> ClientConnectionStore e m
  }

endSession :: SessionID -> Updates (Event ch con) m
endSession sessionID@SessionID {sid, cid} = Updates endSub
  where
    endSub (ClientConnectionStore connections sessions channels) =
      ClientConnectionStore
        { clientConnections = HM.adjust f cid connections,
          clientSessions = HM.delete sessionID sessions,
          activeChannels = channels
        }
    f conn =
      conn
        { connectionSessionIds = filter (/= sid) (connectionSessionIds conn)
        }

startSession ::
  Monad m =>
  Channel e ->
  (e -> m GQLResponse) ->
  SessionID ->
  Updates e m
startSession sessionChannel resolver sessionId = Updates startSub
  where
    startSub ClientConnectionStore {..} =
      ClientConnectionStore
        { clientSessions =
            HM.insert
              sessionId
              ClientSession
                { sessionChannel,
                  sessionCallback =
                    resolver
                      >=> upd . toApolloResponse (sid sessionId)
                }
              clientSessions,
          ..
        }
      where
        upd = mapAt (const (pure ())) connectionCallback (cid sessionId) clientConnections

-- stores active client connections
-- every registered client has ID
-- when client connection is closed client(including all its subscriptions) can By removed By its ID
data ClientConnectionStore e (m :: * -> *) where
  ClientConnectionStore ::
    { clientConnections :: HashMap UUID (ClientConnection m),
      clientSessions :: HashMap SessionID (ClientSession (Event channel content) m),
      activeChannels :: HashMap channel SessionID
    } ->
    ClientConnectionStore (Event channel content) m

deriving instance
  Show e =>
  Show (ClientConnectionStore (Event e c) m)

type StoreMap e m =
  ClientConnectionStore e m ->
  ClientConnectionStore e m

toList :: ClientConnectionStore (Event channel content) m -> [(UUID, ClientConnection m)]
toList = HM.toList . clientConnections

instance KeyOf UUID (ClientConnection m) where
  keyOf = connectionId

instance Collection (ClientConnection m) (ClientConnectionStore (Event ch con) m) where
  empty = ClientConnectionStore empty HM.empty HM.empty
  singleton x = ClientConnectionStore (singleton x) HM.empty HM.empty

mapConnections ::
  ( HashMap UUID (ClientConnection m) -> HashMap UUID (ClientConnection m)
  ) ->
  ClientConnectionStore e m ->
  ClientConnectionStore e m
mapConnections f ClientConnectionStore {..} =
  ClientConnectionStore
    { clientConnections = f clientConnections,
      ..
    }

-- returns original store, if connection with same id already exist
insert ::
  UUID ->
  (ByteString -> m ()) ->
  StoreMap e m
insert connectionId connectionCallback =
  mapConnections (HM.insertWith (const id) connectionId c)
  where
    c =
      ClientConnection
        { connectionId,
          connectionCallback,
          connectionSessionIds = empty
        }

delete ::
  UUID ->
  StoreMap e m
delete key = mapConnections (HM.delete key)
