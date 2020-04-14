{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}



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
  , IN
  , OUT
  , mapS
  , runInput
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
                                                , receiveData
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
 
connect :: MonadIO m => ref -> IO (Stream IN ref e m)
connect clientConnection = do
  clientID <- nextRandom
  let client = Client { clientID , clientConnection, clientSessions = HM.empty }
  return $ Stream [Init client,Update (insert clientID client)] [client]

disconnectClient :: Client ref e m -> Action mode ref e m 
disconnectClient Client { clientID }  = Update $ delete clientID

updateClient
  :: (Client ref e m -> Client ref e m ) 
  -> Client  ref e m 
  -> Action mode ref e m 
updateClient  f Client { clientID } = Update (adjust f clientID)

publishEvents
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Stream OUT ref e m 
publishEvents = initStream . publishEvent

publishEvent
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Action OUT ref e m 
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

endSubscription :: SesionID ->  Client ref e m -> Action mode ref e m 
endSubscription sid = updateClient endSub
 where
  endSub client = client { clientSessions = HM.delete sid (clientSessions client) }

startSubscription :: SubEvent e m -> SesionID -> Client ref e m -> Action mode ref e m 
startSubscription  subscriptions sid = updateClient startSub
 where
  startSub client = client { clientSessions = HM.insert sid subscriptions (clientSessions client) }

data Notification ref m = 
  Notification ref (m ByteString)

data Mode = In | Out

type IN = 'In 
type OUT = 'Out 

data Action 
    (mode :: Mode)
    ref 
    e 
    (m :: * -> * )
  where 
    Error  :: String -> Action mode ref e m
    Update :: (PubSubStore ref e m -> PubSubStore ref e m) -> Action mode ref e m 
    Notify :: (PubSubStore ref e m -> [Notification ref m]) -> Action OUT ref e m
    Init   :: Client ref e m -> Action IN ref e m
    Receive:: Client ref e m -> m ByteString -> Action IN ref e m
    Ignore :: Action mode ref e m


data Stream (mode :: Mode) ref e m = 
  Stream 
    { stream :: [Action mode ref e m]
    , active :: [Client ref e m]
    } 

initStream :: Action mode ref e m -> Stream mode ref e m 
initStream x = Stream [x] []

concatStream :: [Stream mode ref e m ] -> Stream mode ref e m 
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
  -> Client ref e m
  -> m (Action OUT ref e m)
handleSubscription sessionId resStream cl@Client { clientConnection }
  = do
    response <- runResultT resStream
    case response of
      Success { events } -> pure $ execute (head events) -- TODO: solve for liste
      Failure errors     
        -> pure notifyError
       where
         notifyError = Notify 
                $ const
                [ Notification 
                    clientConnection
                    (pure $ toApolloResponse sessionId $ Errors errors)
                ]
 where
  execute (Publish   pub) = publishEvent pub
  execute (Subscribe sub) = startSubscription sub sessionId cl 

apolloToAction 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
        -> ResponseStream e m (Value VALID)
     )
  -> Client ref e m
  -> SubAction  
  -> m (Action OUT ref e m)
apolloToAction _  _ (SubError x) = pure $ Error x
apolloToAction gqlApp client(AddSub sessionId request) 
  = handleSubscription sessionId (gqlApp request) client
apolloToAction _ client (RemoveSub sessionId)
  = pure $ endSubscription sessionId client

disconnect :: Stream mode ref e m -> Stream mode ref e m
disconnect (Stream _ active) = Stream (map disconnectClient active) []

-- EXECUTION
notify :: MonadIO m => Notification Connection m -> m ()
notify (Notification connection msg) = msg >>= liftIO . sendTextData connection

readState :: (MonadIO m) => GQLState ref e m -> m (PubSubStore ref e m)
readState = liftIO . readMVar 

modifyState_ :: (MonadIO m) => GQLState ref e m -> (PubSubStore ref e m -> PubSubStore ref e m) -> m ()
modifyState_ state update = liftIO $ modifyMVar_ state (return . update)

receive :: MonadIO m => Client Connection e m -> m ByteString
receive Client { clientConnection } = liftIO $ receiveData clientConnection

class (MonadIO m, Applicative m) => RunAction ref m where
  runAction :: GQLState ref e m -> Action mode ref e m -> m ()

instance (MonadIO m, Applicative m) => RunAction Connection m where
  runAction state (Update update)  
    = modifyState_ state update 
  runAction state (Notify toNotification)  
    = readState state 
      >>= traverse_ notify  
        . toNotification
  runAction _ (Error x) = liftIO (print x)
  runAction _ Ignore = pure ()
  
runStream :: (RunAction ref m) => Stream mode ref e m -> GQLState ref e m ->  m ()
runStream Stream { stream } state = traverse_ (runAction state) stream

execStream :: (RunAction ref m) => Stream mode ref e m -> GQLState ref e m -> m (Stream mode ref e m)
execStream Stream { stream , active } state 
  = traverse_ (runAction state) stream 
    $> Stream { stream = [] , active }

-- TODO: generic
runInput :: (RunAction Connection m) => GQLState Connection e m -> Action IN Connection e m -> m (Action IN Connection e m)
runInput state (Init client) 
  = runAction state (Update $ insert (clientID client) client) 
    >> pure (Receive client (receive client))
runInput _ x = pure x

initApolloStream 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
     -> ResponseStream e m (Value VALID)
     )
  ->  Action IN ref e m 
  -> m (Action OUT ref e m)
initApolloStream app (Receive cl txt)
  = txt 
    >>= apolloToAction 
          app
          cl
          . apolloFormat
initApolloStream _ _ = pure Ignore

mapS 
  ::(Monad m)
  => ( Action mode ref e m 
     -> m (Action mode' ref e m)
     )
  -> Stream mode ref e m 
  -> m (Stream mode' ref e m)
mapS f Stream { stream , active } = do
    stream' <- traverse f stream
    pure $ Stream { stream = stream', active}
