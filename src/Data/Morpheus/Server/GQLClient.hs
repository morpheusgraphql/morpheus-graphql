module Data.Morpheus.Server.GQLClient
  ( GQLClient(..)
  , ClientID
  , Channel
  ) where

import           Data.Text          (Text)
import           Network.WebSockets (Connection)

instance Show Connection where
  show = const "Connection"

type ClientID = Int

type Channel = Text

data GQLClient = GQLClient
  { clientID         :: ClientID
  , clientConnection :: Connection
  , clientChannels   :: [Channel]
  } deriving (Show)
