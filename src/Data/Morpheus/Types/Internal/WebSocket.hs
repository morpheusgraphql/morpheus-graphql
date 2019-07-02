{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , Channel
  , OutputAction(..)
  , ClientSession(..)
  ) where

import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.IO                     (GQLResponse (..))
import           Data.Semigroup                             ((<>))
import           Data.Text                                  (Text)
import           Data.UUID                                  (UUID)
import           Network.WebSockets                         (Connection)

data OutputAction m s a
  = PublishMutation { mutationChannels                 :: [s]
                    , mutationResponse                 :: a
                    , currentSubscriptionStateResolver :: SelectionSet -> m GQLResponse }
  | InitSubscription { subscriptionChannels :: [s]
                     , subscriptionQuery    :: SelectionSet }
  | NoAction a
  deriving (Functor)

type ClientID = UUID

type Channel = Text

data ClientSession = ClientSession
  { sessionId             :: Int
  , sessionChannels       :: [Channel]
  , sessionQuerySelection :: SelectionSet
  } deriving (Show)

data GQLClient = GQLClient
  { clientID         :: ClientID
  , clientConnection :: Connection
  , clientSessions   :: [ClientSession]
  }

instance Show GQLClient where
  show GQLClient {clientID, clientSessions} =
    "GQLClient {id:" <> show clientID <> ", sessions:" <> show clientSessions <> "}"
