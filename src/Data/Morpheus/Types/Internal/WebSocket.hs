{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.WebSocket
  ( GQLClient(..)
  , ClientID
  , ClientDB
  , GQLState
  , SesionID
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Network.WebSockets             ( Connection )
import           Data.HashMap.Lazy              ( HashMap , keys)
import           Control.Concurrent             ( MVar )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Resolving
                                                ( SubEvent )


-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState m e = MVar (ClientDB m e) -- SharedState

type ClientDB m e = HashMap ClientID (GQLClient m e)

type ClientID = UUID

type SesionID = Text

data GQLClient m e  =
  GQLClient
    { clientID         :: ClientID
    , clientConnection :: Connection
    , clientSessions   :: HashMap SesionID (SubEvent m e)
    }

instance (Show e) => Show (GQLClient m e) where
  show GQLClient { clientID } =
    "GQLClient {id:"
      <> show clientID
      <> "}"
