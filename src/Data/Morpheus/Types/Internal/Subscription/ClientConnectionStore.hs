{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Subscription.ClientConnectionStore
  ( ID,
    Session,
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

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (traverse_)
import Data.HashMap.Lazy (HashMap, keys)
import qualified Data.HashMap.Lazy as HM
  ( adjust,
    delete,
    elems,
    empty,
    insert,
    insertWith,
    keys,
    toList,
  )
import Data.List (intersect)
-- MORPHEUS

import Data.Morpheus.Internal.Utils
  ( Collection (..),
    KeyOf (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Event (..),
    SubEvent,
    eventChannels,
  )
import Data.Morpheus.Types.Internal.Subscription.Apollo
  ( toApolloResponse,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.UUID (UUID)

type ID = UUID

type SessionID = Text

type Session = (ID, SessionID)

data ClientConnection e (m :: * -> *) = ClientConnection
  { connectionId :: ID,
    connectionCallback :: ByteString -> m (),
    -- one connection can have multiple subsciprion session
    connectionSessions :: HashMap SessionID (SubEvent e m)
  }

connectionSessionIds :: ClientConnection e m -> [SessionID]
connectionSessionIds = HM.keys . connectionSessions

instance Show (ClientConnection e m) where
  show ClientConnection {connectionId, connectionSessions} =
    "Connection { id: "
      <> show connectionId
      <> ", sessions: "
      <> show (keys connectionSessions)
      <> " }"

publish ::
  ( Monad m,
    Eq channel
  ) =>
  Event channel content ->
  ClientConnectionStore (Event channel content) m ->
  m ()
publish event = traverse_ sendMessage . elems
  where
    sendMessage ClientConnection {connectionSessions, connectionCallback}
      | null connectionSessions = pure ()
      | otherwise = traverse_ send (filterByChannels connectionSessions)
      where
        send (sid, Event {content = subscriptionRes}) =
          subscriptionRes event >>= connectionCallback . toApolloResponse sid
        ---------------------------
        filterByChannels =
          filter
            ( not
                . null
                . intersect (eventChannels event)
                . channels
                . snd
            )
            . HM.toList

newtype Updates e (m :: * -> *) = Updates
  { _runUpdate :: ClientConnectionStore e m -> ClientConnectionStore e m
  }

updateClient ::
  (ClientConnection e m -> ClientConnection e m) ->
  ID ->
  Updates e m
updateClient f cid = Updates (adjust f cid)

endSession :: Session -> Updates e m
endSession (clientId, sessionId) = updateClient endSub clientId
  where
    endSub client = client {connectionSessions = HM.delete sessionId (connectionSessions client)}

startSession :: SubEvent e m -> Session -> Updates e m
startSession subscriptions (clientId, sessionId) = updateClient startSub clientId
  where
    startSub client = client {connectionSessions = HM.insert sessionId subscriptions (connectionSessions client)}

-- stores active client connections
-- every registered client has ID
-- when client connection is closed client(including all its subsciprions) can By removed By its ID
newtype ClientConnectionStore e (m :: * -> *) = ClientConnectionStore
  { unpackStore :: HashMap ID (ClientConnection e m)
  }
  deriving (Show)

type StoreMap e m =
  ClientConnectionStore e m ->
  ClientConnectionStore e m

mapStore ::
  ( HashMap ID (ClientConnection e m) ->
    HashMap ID (ClientConnection e m)
  ) ->
  StoreMap e m
mapStore f = ClientConnectionStore . f . unpackStore

elems :: ClientConnectionStore e m -> [ClientConnection e m]
elems = HM.elems . unpackStore

toList :: ClientConnectionStore e m -> [(UUID, ClientConnection e m)]
toList = HM.toList . unpackStore

instance KeyOf UUID (ClientConnection e m) where
  keyOf = connectionId

instance Collection (ClientConnection e m) (ClientConnectionStore e m) where
  empty = ClientConnectionStore empty
  singleton = ClientConnectionStore . singleton

-- returns original store, if connection with same id already exist
insert ::
  ID ->
  (ByteString -> m ()) ->
  StoreMap e m
insert connectionId connectionCallback = mapStore (HM.insertWith (curry snd) connectionId c)
  where
    c = ClientConnection {connectionId, connectionCallback, connectionSessions = HM.empty}

adjust ::
  (ClientConnection e m -> ClientConnection e m) ->
  ID ->
  StoreMap e m
adjust f key = mapStore (HM.adjust f key)

delete ::
  ID ->
  StoreMap e m
delete key = mapStore (HM.delete key)
