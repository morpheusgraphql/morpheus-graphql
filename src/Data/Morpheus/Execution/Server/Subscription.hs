{-# LANGUAGE NamedFieldPuns , FlexibleContexts #-}

module Data.Morpheus.Execution.Server.Subscription
  ( ClientDB
  , GQLState
  , initGQLState
  , connectClient
  , disconnectClient
  , startSubscription
  , endSubscription
  , publishEvent
  , Actions(..)
  )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.ByteString.Lazy.Char8     (ByteString)
import           Control.Concurrent             ( modifyMVar
                                                , modifyMVar_
                                                , newMVar
                                                )
import           Data.List                      ( intersect )
import           Data.UUID.V4                   ( nextRandom )
import           Network.WebSockets             ( Connection )
import           Data.HashMap.Lazy              ( empty
                                                , insert
                                                , delete
                                                , adjust
                                                , toList
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Apollo
                                                ( toApolloResponse )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Event(..)
                                                , GQLChannel(..)
                                                , SubEvent
                                                )
import           Data.Morpheus.Types.Internal.WebSocket
                                                ( ClientID
                                                , GQLClient(..)
                                                , ClientDB
                                                , GQLState
                                                , SesionID
                                                )

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState m e)
initGQLState = newMVar empty
 
connectClient :: MonadIO m => Connection -> GQLState m e -> IO (GQLClient m e)
connectClient clientConnection gqlState = do
  clientID <- nextRandom
  let client = GQLClient { clientID , clientConnection, clientSessions = empty }
  modifyMVar_ gqlState (pure . insert clientID client)
  return client

disconnectClient :: GQLClient m e -> GQLState m e -> IO (ClientDB m e)
disconnectClient GQLClient { clientID } state = modifyMVar state removeUser
 where
  removeUser db = let s' = delete clientID db in return (s', s')

updateClientByID
  :: ClientID
  -> (GQLClient m e -> GQLClient m e) 
  -> Stream m e
updateClientByID key f = [Update (adjust f key)]

publishEvent
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Stream m e
publishEvent event = [Notify (concatMap sendMessage . toList) ]
 where
  sendMessage (_,GQLClient { clientSessions, clientConnection })
    | null clientSessions  = [] 
    | otherwise = map send (filterByChannels clientSessions)
   where
    send (sid, Event { content = subscriptionRes }) 
      =  (clientConnection, toApolloResponse sid <$> subscriptionRes event) 
    ---------------------------
    filterByChannels = filter
      ( not
      . null
      . intersect (streamChannels event)
      . channels
      . snd
      ) . toList

endSubscription :: ClientID -> SesionID -> Stream m e
endSubscription cid sid = updateClientByID cid endSub
 where
  endSub client = client { clientSessions = delete sid (clientSessions client) }

startSubscription :: ClientID -> SubEvent m e -> SesionID -> Stream m e
startSubscription cid subscriptions sid = updateClientByID cid startSub
 where
  startSub client = client { clientSessions = insert sid subscriptions (clientSessions client) }


type Stream m e = [Actions m e]

data Actions m e
  = Update (ClientDB m e -> ClientDB m e)
  | Notify (ClientDB m e -> [(Connection, m ByteString)])
  | Register (m ())