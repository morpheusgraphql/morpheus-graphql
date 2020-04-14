{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}

module Data.Morpheus.Execution.Server.Subscription
  ( ClientDB
  , GQLState
  , initGQLState
  , connectClient
  , disconnectClient
  , startSubscription
  , endSubscription
  , publishEvent
  , Action(..)
  , runStream
  , Stream(..)
  , handleSubscription
  )
where


import           Data.Functor                   (($>))
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.ByteString.Lazy.Char8     (ByteString)
import           Control.Concurrent             ( newMVar 
                                                , modifyMVar_
                                                , readMVar
                                                )
import           Data.List                      ( intersect )
import           Data.UUID.V4                   ( nextRandom )
import           Network.WebSockets             ( Connection 
                                                , sendTextData
                                                )
import           Data.HashMap.Lazy              ( empty
                                                , insert
                                                , delete
                                                , adjust
                                                , toList
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST
                                                ( Value
                                                , VALID
                                                , Name
                                                )
import           Data.Morpheus.Types.IO         (GQLResponse(..))
import           Data.Morpheus.Types.Internal.Apollo
                                                ( toApolloResponse 
                                                , SubAction(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Event(..)
                                                , GQLChannel(..)
                                                , SubEvent
                                                , GQLChannel(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , runResultT
                                                , Result(..)
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
 
connectClient :: MonadIO m => Connection -> IO (Stream m e, GQLClient m e)
connectClient clientConnection = do
  clientID <- nextRandom
  let client = GQLClient { clientID , clientConnection, clientSessions = empty }
  return (Stream [Update (insert clientID client)], client)

disconnectClient :: GQLClient m e -> Stream m e
disconnectClient GQLClient { clientID }  = Stream [Update (delete clientID)]

updateClientByID
  :: ClientID
  -> (GQLClient m e -> GQLClient m e) 
  -> Stream m e
updateClientByID key f = Stream [Update (adjust f key)]

publishEvent
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Stream m e
publishEvent event = Stream [ Notify $ concatMap sendMessage . toList ]
 where
  sendMessage (_,GQLClient { clientSessions, clientConnection })
    | null clientSessions  = [] 
    | otherwise = map send (filterByChannels clientSessions)
   where
    send (sid, Event { content = subscriptionRes }) 
      =  Notificaion 
          clientConnection 
          (toApolloResponse sid <$> subscriptionRes event)
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

data Notificaion m = 
  Notificaion Connection (m ByteString)

data Action m e
  = Update (ClientDB m e -> ClientDB m e)
  | Notify (ClientDB m e -> [Notificaion m])
  | Connected (GQLClient m e)
  | Log String

instance Show (Action m e) where
  show Update {} = "Update"
  show Notify {} = "Notify"
  show Connected {} = "Connected"

newtype Stream m e = 
  Stream 
    { stream :: [Action m e] 
    } deriving (Show, Semigroup)

collectStream :: (a -> Stream m e) -> [a] -> Stream m e
collectStream f x = Stream $ concatMap (stream . f)  x

handleSubscription
  :: (Eq (StreamChannel e), GQLChannel e, MonadIO m)
  => GQLClient m e
  -> Name
  -> ResponseStream e m (Value VALID)
  -> m (Stream m e)
handleSubscription GQLClient { clientConnection, clientID } sessionId stream
  = do
    response <- runResultT stream
    case response of
      Success { events } -> pure $ collectStream execute events
      Failure errors     
        -> pure 
            $ Stream
              [ Notify 
                $ const
                [ Notificaion 
                    clientConnection
                    (pure $ toApolloResponse sessionId $ Errors errors)
                ]
              ]

 where
  execute :: (Eq (StreamChannel e) , GQLChannel e, Functor m ) => ResponseEvent m e -> Stream m e
  execute (Publish   pub) = publishEvent pub
  execute (Subscribe sub) = startSubscription clientID sub sessionId

-- EXECUTION
notify :: MonadIO m => Notificaion m -> m ()
notify (Notificaion connection msg) = msg >>= liftIO . sendTextData connection

readState :: (MonadIO m) => GQLState m e -> m (ClientDB m e)
readState = liftIO . readMVar 

modifyState_ :: (MonadIO m) => GQLState m e -> (ClientDB m e -> ClientDB m e) -> m ()
modifyState_ state update = liftIO $ modifyMVar_ state (return . update)

runAction :: (MonadIO m) => GQLState m e -> Action m e -> m ()
runAction state (Update update)  
  = modifyState_ state update 
runAction state (Notify toNotification)  
  = readState state 
    >>= traverse_ notify  
      . toNotification
runAction _ (Connected x) = pure () -- TODO: real connection due Stream
runAction _ (Log x) = liftIO (print x)

runStream :: (MonadIO m) => Stream m e -> GQLState m e ->  m ()
runStream Stream { stream } state = traverse_ (runAction state) stream
