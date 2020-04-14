{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE FlexibleContexts     #-}

module Data.Morpheus.Execution.Server.Subscription
  ( ClientDB
  , GQLState
  , initGQLState
  , connectClient
  , disconnectClient
  , startSubscription
  , publishEvent
  , Action(..)
  , runStream
  , Stream(..)
  , initApolloStream
  , execStream
  , disconnect
  , collectStream
  , concatStream
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
import           Data.Morpheus.Types.IO         ( GQLResponse(..)
                                                , GQLRequest(..)
                                                )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( toApolloResponse 
                                                , SubAction(..)
                                                , apolloFormat
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
 
connectClient :: MonadIO m => Connection -> IO (Stream m e)
connectClient clientConnection = do
  clientID <- nextRandom
  let client = GQLClient { clientID , clientConnection, clientSessions = empty }
  return $ 
      Stream 
        [Update (insert clientID client)] 
        [client]

disconnectClient :: GQLClient m e -> Action m e
disconnectClient GQLClient { clientID }  = Update $ delete clientID

updateClientByID
  :: ClientID
  -> (GQLClient m e -> GQLClient m e) 
  -> Stream m e
updateClientByID key f = initStream $ Update $ adjust f key

publishEvent
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Stream m e
publishEvent event = initStream $ Notify $ concatMap sendMessage . toList
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

  

endSubscription :: SesionID -> Stream m e ->  Stream m e
endSubscription sid Stream { stream , active } = do
  let (Stream stream' active') = collectStream (endSubscriptionByClient sid . clientID ) active
  Stream (stream <> stream') (active <> active')

endSubscriptionByClient :: SesionID ->  ClientID -> Stream m e
endSubscriptionByClient sid cid = updateClientByID cid endSub
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
  | Log String

instance Show (Action m e) where
  show Update {} = "Update"
  show Notify {} = "Notify"
  show Log {}    = "Log"

data Stream m e = 
  Stream 
    { stream :: [Action m e]
    , active :: [GQLClient m e]
    } deriving (Show)

initStream :: Action m e -> Stream m e 
initStream x = Stream [x] []

concatStream :: [Stream m e] -> Stream m e
concatStream xs = Stream 
      (concatMap stream xs) 
      (concatMap active xs)

collectStream :: (a -> Stream m e) -> [a] -> Stream m e
collectStream f xs = concatStream (map f xs)

handleSubscription
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => Name
  -> ResponseStream e m (Value VALID)
  -> Stream m e
  -> m (Stream m e)
handleSubscription sessionId resStream Stream { stream, active }
  = do
    response <- runResultT resStream
    case response of
      Success { events } -> pure $ collectStream execute events
      Failure errors     
        -> pure 
            $ Stream 
              (stream <> map notifyError active)
              []
       where
         notifyError GQLClient {clientConnection } = Notify 
                $ const
                [ Notificaion 
                    clientConnection
                    (pure $ toApolloResponse sessionId $ Errors errors)
                ]
 where
  execute :: (Eq (StreamChannel e) , GQLChannel e, Functor m ) => ResponseEvent m e -> Stream m e
  execute (Publish   pub) = publishEvent pub
  execute (Subscribe sub) = collectStream start active
    where
      start GQLClient { clientID } = startSubscription clientID sub sessionId

apolloToAction 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
        -> ResponseStream e m (Value VALID)
     )
  -> SubAction  
  -> Stream m e 
  -> m (Stream m e) 
apolloToAction _ (SubError x) = const $ pure $ initStream $ Log x
apolloToAction gqlApp (AddSub sessionId request) 
  = handleSubscription sessionId (gqlApp request)
apolloToAction _ (RemoveSub sessionId)
  = pure . endSubscription sessionId 

initApolloStream 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
        -> ResponseStream e m (Value VALID)
     )
  -> Stream m e
  -> ByteString
  -> m (Stream m e) 
initApolloStream gqlApp s input 
  = apolloToAction 
      gqlApp
      (apolloFormat input)
      s

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
runAction _ (Log x) = liftIO (print x)

runStream :: (MonadIO m) => Stream m e -> GQLState m e ->  m ()
runStream Stream { stream } state = traverse_ (runAction state) stream

execStream :: (MonadIO m) => Stream m e -> GQLState m e ->  m (Stream m e)
execStream Stream { stream , active } state 
  = traverse_ (runAction state) stream 
    $> Stream { stream = [] , active }

disconnect :: Stream m e -> Stream m e
disconnect (Stream stream active) = Stream (stream <> map disconnectClient active) []
