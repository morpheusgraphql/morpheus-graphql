{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}


module Data.Morpheus.Execution.Server.Subscription
  ( ClientDB
  , GQLState
  , initGQLState
  , connect
  , disconnect
  , publishEvents
  , runStream
  , Stream(..)
  , initApolloStream
  , execStream
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
                                                ( GQLClient(..)
                                                , ClientDB
                                                , GQLState
                                                , SesionID
                                                )

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState m e)
initGQLState = newMVar empty
 
connect :: MonadIO m => Connection -> IO (Stream m e)
connect clientConnection = do
  clientID <- nextRandom
  let client = GQLClient { clientID , clientConnection, clientSessions = empty }
  return $ 
      Stream 
        [Update (insert clientID client)] 
        [client]

disconnectClient :: GQLClient m e -> Action m e
disconnectClient GQLClient { clientID }  = Update $ delete clientID

updateClient
  :: (GQLClient m e -> GQLClient m e) 
  -> GQLClient m e
  -> Action m e
updateClient  f GQLClient { clientID } = Update (adjust f clientID)

publishEvents
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Stream m e
publishEvents = initStream . publishEvent

publishEvent
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Action m e
publishEvent event = Notify (concatMap sendMessage . toList)
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
endSubscription sid Stream { stream , active } 
  = Stream 
      (stream <> map (endSubscriptionByClient sid) active) 
      active

endSubscriptionByClient :: SesionID ->  GQLClient m e -> Action m e
endSubscriptionByClient sid = updateClient endSub
 where
  endSub client = client { clientSessions = delete sid (clientSessions client) }

startSubscription :: SubEvent m e -> SesionID -> GQLClient m e -> Action m e
startSubscription  subscriptions sid = updateClient startSub
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
      Success { events } -> pure $ Stream (stream <> concatMap execute events) active
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
  execute (Publish   pub) = [publishEvent pub]
  execute (Subscribe sub) = stream <> map (startSubscription sub sessionId) active 

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

disconnect :: Stream m e -> Stream m e
disconnect (Stream stream active) = Stream (stream <> map disconnectClient active) []

-- EXECUTION
notify :: MonadIO m => Notificaion m -> m ()
notify (Notificaion connection msg) = msg >>= liftIO . sendTextData connection

readState :: (MonadIO m) => GQLState m e -> m (ClientDB m e)
readState = liftIO . readMVar 

modifyState_ :: (MonadIO m) => GQLState m e -> (ClientDB m e -> ClientDB m e) -> m ()
modifyState_ state update = liftIO $ modifyMVar_ state (return . update)

class (MonadIO m, Applicative m) => RunAction m where
  runAction :: GQLState m e -> Action m e -> m ()

instance (MonadIO m, Applicative m) => RunAction m where
  runAction state (Update update)  
    = modifyState_ state update 
  runAction state (Notify toNotification)  
    = readState state 
      >>= traverse_ notify  
        . toNotification
  runAction _ (Log x) = liftIO (print x)

runStream :: (Applicative m, MonadIO m) => Stream m e -> GQLState m e ->  m ()
runStream Stream { stream } state = traverse_ (runAction state) stream

execStream :: (Applicative m, MonadIO m) => Stream m e -> GQLState m e ->  m (Stream m e)
execStream Stream { stream , active } state 
  = traverse_ (runAction state) stream 
    $> Stream { stream = [] , active }


