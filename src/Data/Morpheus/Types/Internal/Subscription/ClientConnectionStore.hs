{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.Subscription.ClientConnectionStore
  ( ID,
    Session,
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
    keys,
    toList,
  )
import Data.List (intersect)
-- MORPHEUS

import Data.Morpheus.Types.Internal.Operation
  ( Empty (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Event (..),
    GQLChannel (..),
    SubEvent,
  )
import Data.Morpheus.Types.Internal.Subscription.Apollo
  ( toApolloResponse,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.UUID (UUID)

type ID = UUID

type SesionID = Text

type Session = (ID, SesionID)

data ClientConnection e (m :: * -> *) = ClientConnection
  { connectionId :: ID,
    connectionCallback :: ByteString -> m (),
    -- one connection can have multiple subsciprion session
    connectionSessions :: HashMap SesionID (SubEvent e m)
  }

connectionSessionIds :: ClientConnection e m -> [SesionID]
connectionSessionIds = HM.keys . connectionSessions

instance Show (ClientConnection e m) where
  show ClientConnection {connectionId, connectionSessions} =
    "Connection { id: "
      <> show connectionId
      <> ", sessions: "
      <> show (keys connectionSessions)
      <> " }"

publish ::
  ( Eq (StreamChannel event),
    GQLChannel event,
    Monad m
  ) =>
  event ->
  ClientConnectionStore event m ->
  m ()
publish event = traverse_ sendMessage . elems
  where
    sendMessage ClientConnection {connectionSessions, connectionCallback}
      | null connectionSessions = pure ()
      | otherwise = traverse_ send (filterByChannels connectionSessions)
      where
        send (sid, Event {content = subscriptionRes}) =
          toApolloResponse sid <$> subscriptionRes event >>= connectionCallback
        ---------------------------
        filterByChannels =
          filter
            ( not
                . null
                . intersect (streamChannels event)
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

instance Empty (ClientConnectionStore e m) where
  empty = ClientConnectionStore HM.empty

insert ::
  ID ->
  (ByteString -> m ()) ->
  StoreMap e m
insert connectionId connectionCallback = mapStore (HM.insert connectionId c)
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
