{-# LANGUAGE DeriveFunctor #-}

module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , Channel
  , OutputAction(..)
  , ClientSession(..)
  ) where

import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Text                                  (Text)
import           Data.UUID                                  (UUID)
import           Network.WebSockets                         (Connection)

instance Show Connection where
  show = const "Connection"

data OutputAction a
  = PublishMutation { mutationChannels                 :: [Text]
                    , mutationResponse                 :: a
                    , currentSubscriptionStateResolver :: SelectionSet -> IO GQLResponse }
  | InitSubscription { subscriptionChannels :: [Text]
                     , subscriptionQuery    :: SelectionSet }
  | NoEffect a
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
  } deriving (Show)
