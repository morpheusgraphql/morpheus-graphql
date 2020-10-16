{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Subscription.ClientConnectionStore
  ( Session,
    SessionID,
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

import Control.Applicative (pure)
import Control.Monad ((>=>), Monad ((>>=)))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (traverse_)
import Data.HashMap.Lazy (HashMap, keys)
import qualified Data.HashMap.Lazy as HM
import Data.List (filter, intersect)
import Data.Maybe (maybe)
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    KeyOf (..),
  )
import Data.Morpheus.Types.IO (GQLResponse)
import Data.Morpheus.Types.Internal.Resolving
  ( Channel (..),
    Event (..),
    SubEvent,
    eventChannels,
  )
import Data.Morpheus.Types.Internal.Subscription.Apollo
  ( toApolloResponse,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.UUID (UUID)
import Prelude
  ( (.),
    Eq (..),
    Eq (..),
    Show (..),
    const,
    elem,
    id,
    not,
    null,
    otherwise,
    snd,
  )

type SessionID = Text

type Session = (UUID, SessionID)

data ClientConnection e (m :: * -> *) = ClientConnection
  { connectionId :: UUID,
    connectionCallback :: ByteString -> m (),
    -- one connection can have multiple subscription session
    connectionSessionIds :: [SessionID]
  }

data ClientSession e (m :: * -> *) = ClientSession
  { sessionId :: SessionID,
    sessionChannel :: Channel e,
    sessionCallback :: e -> m ()
  }

instance Show (ClientSession e m) where
  show ClientSession {sessionId} = "ClientSession { id = " <> show sessionId <> " }"

instance Show (ClientConnection e m) where
  show ClientConnection {connectionId, connectionSessionIds} =
    "ClientConnection { id = "
      <> show connectionId
      <> ", sessions = "
      <> show connectionSessionIds
      <> " }"

publish ::
  ( Monad m,
    Eq channel
  ) =>
  Event channel content ->
  ClientConnectionStore (Event channel content) m ->
  m ()
publish event = traverse_ sendMessage . clientSessions
  where
    sendMessage ClientSession {sessionChannel, sessionCallback}
      | sessionChannel `elem` eventChannels event = sessionCallback event
      | otherwise = pure ()

newtype Updates e (m :: * -> *) = Updates
  { _runUpdate :: ClientConnectionStore e m -> ClientConnectionStore e m
  }

endSession :: Session -> Updates e m
endSession (clientId, sid) = Updates endSub
  where
    endSub store =
      store
        { clientConnections = HM.adjust f clientId (clientConnections store),
          clientSessions = HM.delete (clientId, sid) (clientSessions store)
        }
    f conn =
      conn
        { connectionSessionIds = filter (/= sid) (connectionSessionIds conn)
        }

startSession ::
  Monad m =>
  Channel e ->
  (e -> m GQLResponse) ->
  Session ->
  Updates e m
startSession sessionChannel resolver (clientId, sessionId) = Updates startSub
  where
    startSub store =
      store
        { clientSessions =
            HM.insert
              (clientId, sessionId)
              ClientSession
                { sessionId,
                  sessionChannel,
                  sessionCallback =
                    resolver
                      >=> upd . toApolloResponse sessionId
                }
              (clientSessions store)
        }
      where
        upd = maybe (const (pure ())) connectionCallback (HM.lookup clientId (clientConnections store))

-- stores active client connections
-- every registered client has ID
-- when client connection is closed client(including all its subscriptions) can By removed By its ID
data ClientConnectionStore e (m :: * -> *) = ClientConnectionStore
  { clientConnections :: HashMap UUID (ClientConnection e m),
    clientSessions :: HashMap (UUID, SessionID) (ClientSession e m)
  }
  deriving (Show)

type StoreMap e m =
  ClientConnectionStore e m ->
  ClientConnectionStore e m

mapStore ::
  ( HashMap UUID (ClientConnection e m) ->
    HashMap UUID (ClientConnection e m)
  ) ->
  StoreMap e m
mapStore f (ClientConnectionStore x y) = ClientConnectionStore (f x) y

toList :: ClientConnectionStore e m -> [(UUID, ClientConnection e m)]
toList = HM.toList . clientConnections

instance KeyOf UUID (ClientConnection e m) where
  keyOf = connectionId

-- instance Collection (ClientConnection e m) (ClientConnectionStore e m) where
--   empty = ClientConnectionStore empty
--   singleton = ClientConnectionStore . singleton

-- returns original store, if connection with same id already exist
insert ::
  UUID ->
  (ByteString -> m ()) ->
  StoreMap e m
insert connectionId connectionCallback = mapStore (HM.insertWith (const id) connectionId c)
  where
    c =
      ClientConnection
        { connectionId,
          connectionCallback,
          connectionSessionIds = empty
        }

adjust ::
  (ClientConnection e m -> ClientConnection e m) ->
  UUID ->
  StoreMap e m
adjust f key = mapStore (HM.adjust f key)

delete ::
  UUID ->
  StoreMap e m
delete key = mapStore (HM.delete key)
