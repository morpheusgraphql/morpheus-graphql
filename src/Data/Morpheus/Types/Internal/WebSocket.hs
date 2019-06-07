module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , Channel
  , InputAction(..)
  , OutputAction(..)
  , ClientSession(..)
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

instance Functor OutputAction where
  fmap f (NoEffect result)                      = NoEffect (f result)
  fmap f (PublishMutation clientId' result' y') = PublishMutation clientId' (f result') y'
  fmap _ (InitSubscription x' y' z')            = InitSubscription x' y' z'

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
