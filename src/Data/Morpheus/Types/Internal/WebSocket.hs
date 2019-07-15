{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , ClientSession(..)
  ) where

import           Data.Semigroup                      ((<>))
import           Data.Text                           (Text)
import           Data.UUID                           (UUID)
import           Network.WebSockets                  (Connection)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Stream (SubPair)

type ClientID = UUID

data ClientSession m s = ClientSession
  { sessionId           :: Text
  , sessionSubscription :: SubPair m s
  }

instance Show s => Show (ClientSession m s) where
  show ClientSession {sessionId} = "GQLSession { id: " <> show sessionId <> ", sessions: " <> "" <> " }"

data GQLClient m s = GQLClient
  { clientID         :: ClientID
  , clientConnection :: Connection
  , clientSessions   :: [ClientSession m s]
  }

instance Show s => Show (GQLClient m s) where
  show GQLClient {clientID, clientSessions} =
    "GQLClient {id:" <> show clientID <> ", sessions:" <> show clientSessions <> "}"
