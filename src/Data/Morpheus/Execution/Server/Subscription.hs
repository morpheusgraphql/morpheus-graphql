{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

module Data.Morpheus.Execution.Server.Subscription
  ( Client
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
  , RunAction(..)
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
import qualified Data.HashMap.Lazy   as   HM    ( empty
                                                , toList
                                                , insert
                                                , delete
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
                                                ( Client(..)
                                                , PubSubStore
                                                , GQLState
                                                , SesionID
                                                , concatUnfold
                                                , empty
                                                , insert
                                                , adjust
                                                , delete
                                                )

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState ref e m)
initGQLState = newMVar empty
 
connect :: MonadIO m => ref -> IO (Stream ref e m)
connect clientConnection = do
  clientID <- nextRandom
  let client = Client { clientID , clientConnection, clientSessions = HM.empty }
  return $ 
      Stream 
        [Update (insert clientID client)] 
        [client]

disconnectClient :: Client ref e m -> Action ref e m 
disconnectClient Client { clientID }  = Update $ delete clientID

updateClient
  :: (Client ref e m -> Client ref e m ) 
  -> Client  ref e m 
  -> Action  ref e m 
updateClient  f Client { clientID } = Update (adjust f clientID)

publishEvents
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Stream ref e m 
publishEvents = initStream . publishEvent

publishEvent
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Action ref e m 
publishEvent event = Notify $ concatUnfold sendMessage
 where
  sendMessage Client { clientSessions, clientConnection }
    | null clientSessions  = [] 
    | otherwise = map send (filterByChannels clientSessions)
   where
    send (sid, Event { content = subscriptionRes }) 
      =  Notification 
          clientConnection 
          (toApolloResponse sid <$> subscriptionRes event)
    ---------------------------
    filterByChannels = filter
      ( not
      . null
      . intersect (streamChannels event)
      . channels
      . snd
      ) . HM.toList

endSubscription :: SesionID -> Stream ref e m  ->  Stream ref e m 
endSubscription sid Stream { stream , active } 
  = Stream 
      (stream <> map (endSubscriptionByClient sid) active) 
      active

endSubscriptionByClient :: SesionID ->  Client ref e m -> Action  ref e m 
endSubscriptionByClient sid = updateClient endSub
 where
  endSub client = client { clientSessions = HM.delete sid (clientSessions client) }

startSubscription :: SubEvent e m -> SesionID -> Client ref e m -> Action ref e m 
startSubscription  subscriptions sid = updateClient startSub
 where
  startSub client = client { clientSessions = HM.insert sid subscriptions (clientSessions client) }

data Notification ref m = 
  Notification ref (m ByteString)

data Action 
    ref 
    e 
    (m :: * -> * )
  = Update (PubSubStore ref e m -> PubSubStore ref e m)
  | Notify (PubSubStore ref e m -> [Notification ref m])
  | Log String

instance Show (Action ref e m) where
  show Update {} = "Update"
  show Notify {} = "Notify"
  show Log {}    = "Log"

data Stream ref e m = 
  Stream 
    { stream :: [Action ref e m]
    , active :: [Client ref e m]
    } deriving (Show)

initStream :: Action ref e m -> Stream ref e m 
initStream x = Stream [x] []

concatStream :: [Stream ref e m ] -> Stream ref e m 
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
  -> Stream ref e m 
  -> m (Stream ref e m)
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
         notifyError Client {clientConnection } = Notify 
                $ const
                [ Notification 
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
  -> Stream ref e m 
  -> m (Stream ref e m) 
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
  -> Stream ref e m 
  -> ByteString
  -> m (Stream ref e m) 
initApolloStream gqlApp s input 
  = apolloToAction 
      gqlApp
      (apolloFormat input)
      s

disconnect :: Stream ref e m -> Stream ref e m
disconnect (Stream stream active) = Stream (stream <> map disconnectClient active) []

-- EXECUTION
notify :: MonadIO m => Notification Connection m -> m ()
notify (Notification connection msg) = msg >>= liftIO . sendTextData connection

readState :: (MonadIO m) => GQLState ref e m -> m (PubSubStore ref e m)
readState = liftIO . readMVar 

modifyState_ :: (MonadIO m) => GQLState ref e m -> (PubSubStore ref e m -> PubSubStore ref e m) -> m ()
modifyState_ state update = liftIO $ modifyMVar_ state (return . update)

class (MonadIO m, Applicative m) => RunAction ref m where
  runAction :: GQLState ref e m -> Action ref e m -> m ()

instance (MonadIO m, Applicative m) => RunAction Connection m where
  runAction state (Update update)  
    = modifyState_ state update 
  runAction state (Notify toNotification)  
    = readState state 
      >>= traverse_ notify  
        . toNotification
  runAction _ (Log x) = liftIO (print x)

runStream :: (RunAction ref m) => Stream ref e m -> GQLState ref e m ->  m ()
runStream Stream { stream } state = traverse_ (runAction state) stream

execStream :: (RunAction ref m) => Stream ref e m -> GQLState ref e m ->  m (Stream ref e m)
execStream Stream { stream , active } state 
  = traverse_ (runAction state) stream 
    $> Stream { stream = [] , active }


