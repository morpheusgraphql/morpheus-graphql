module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , Channel
  , InputAction(..)
  , OutputAction(..)
  ) where

import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Text                                  (Text)
import           Data.UUID                                  (UUID)
import           Network.WebSockets                         (Connection)

instance Show Connection where
  show = const "Connection"

data InputAction a = SocketInput
  { connectionID :: ClientID
  , inputValue   :: a
  } deriving (Show)

data OutputAction a
  = PublishMutation { mutationChannels     :: [Text]
                    , mutationResponse     :: a
                    , subscriptionResolver :: SelectionSet -> IO Text }
  | InitSubscription { subscriptionClientID :: ClientID
                     , subscriptionChannels :: [Text]
                     , subscriptionQuery    :: SelectionSet }
  | NoEffect a

type ClientID = UUID

type Channel = Text

data GQLClient = GQLClient
  { clientID             :: ClientID
  , clientConnection     :: Connection
  , clientChannels       :: [Channel]
  , clientSessionId      :: Int
  , clientQuerySelection :: SelectionSet
  } deriving (Show)
