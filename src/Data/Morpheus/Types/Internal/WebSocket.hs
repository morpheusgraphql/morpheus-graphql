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

data ClientSession m e c = ClientSession
  { sessionId           :: Text
  , sessionSubscription :: SubPair m e c
  }

instance (Show e, Show c) => Show (ClientSession m e c) where
  show ClientSession {sessionId} = "GQLSession { id: " <> show sessionId <> ", sessions: " <> "" <> " }"

data GQLClient m e c = GQLClient
  { clientID         :: ClientID
  , clientConnection :: Connection
  , clientSessions   :: [ClientSession m e c]
  }

instance (Show e, Show c) => Show (GQLClient m e c) where
  show GQLClient {clientID, clientSessions} =
    "GQLClient {id:" <> show clientID <> ", sessions:" <> show clientSessions <> "}"
