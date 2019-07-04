{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , OutputAction(..)
  , ClientSession(..)
  , WSSubscription(..)
  ) where

import           Data.Morpheus.Types.IO       (GQLResponse)
import           Data.Morpheus.Types.Resolver (EventContent)
import           Data.Semigroup               ((<>))
import           Data.UUID                    (UUID)
import           Network.WebSockets           (Connection)

data OutputAction m s a
  = PublishMutation { mutationChannels :: [([s], EventContent s)]
                    , mutationResponse :: a }
  | InitSubscription (WSSubscription m s)
  | NoAction a
  deriving (Functor)

type ClientID = UUID

data WSSubscription m s = WSSubscription
  { subscriptionChannels :: [s]
  , subscriptionRes      :: EventContent s -> m GQLResponse
  }

data ClientSession m s = ClientSession
  { sessionId           :: Int
  , sessionSubscription :: WSSubscription m s
  }

instance Show s => Show (ClientSession m s) where
  show ClientSession {sessionId} = "GQLSession {id:" <> show sessionId <> ", sessions:" <> "" <> "}"

data GQLClient m s = GQLClient
  { clientID         :: ClientID
  , clientConnection :: Connection
  , clientSessions   :: [ClientSession m s]
  }

instance Show s => Show (GQLClient m s) where
  show GQLClient {clientID, clientSessions} =
    "GQLClient {id:" <> show clientID <> ", sessions:" <> show clientSessions <> "}"
