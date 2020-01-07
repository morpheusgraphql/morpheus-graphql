{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , ClientSession(..)
  , ClientDB
  , GQLState
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Network.WebSockets             ( Connection )
import           Data.HashMap.Lazy              ( HashMap )
import           Control.Concurrent             ( MVar )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Resolving
                                                ( SubEvent )


-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState m e = MVar (ClientDB m e) -- SharedState

type ClientDB m e = HashMap ClientID (GQLClient m e)

type ClientID = UUID

data ClientSession m e =
  ClientSession
    { sessionId           :: Text
    , sessionSubscription :: SubEvent m e
    }

instance (Show e) => Show (ClientSession m e ) where
  show ClientSession { sessionId } =
    "GQLSession { id: " <> show sessionId <> ", sessions: " <> "" <> " }"

data GQLClient m e  =
  GQLClient
    { clientID         :: ClientID
    , clientConnection :: Connection
    , clientSessions   :: [ClientSession m e ]
    }

instance (Show e) => Show (GQLClient m e) where
  show GQLClient { clientID, clientSessions } =
    "GQLClient {id:"
      <> show clientID
      <> ", sessions:"
      <> show clientSessions
      <> "}"
