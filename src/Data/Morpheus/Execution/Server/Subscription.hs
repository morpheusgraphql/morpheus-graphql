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
  , toResponseStream
  , RunAction(..)
  , IN
  , OUT
  , mapS
  , runInput
  , traverseS
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
import          Debug.Trace 

-- | initializes empty GraphQL state
initGQLState :: IO (GQLState ref e m)
initGQLState = newMVar empty
 
connect :: MonadIO m => ref -> IO (Stream IN ref e m)
connect clientConnection = do
  clientID <- nextRandom
  let client = Client { clientID , clientConnection, clientSessions = HM.empty }
  return $ Stream [Update (insert clientID client), Receive client]

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

instance Show (Notification r m) where
  show _ = "Notification"

data Mode = In | Out

type IN = 'In 
type OUT = 'Out 

data Action 
    (mode :: Mode)
    ref 
    e 
    (m :: * -> * )
  where 
    Error   :: String -> Action mode ref e m
    Update  :: (PubSubStore ref e m -> PubSubStore ref e m) -> Action mode ref e m 
    Notify  :: (PubSubStore ref e m -> [Notification ref m]) -> Action OUT ref e m
    -- Init    :: Client ref e m -> Action IN ref e m
    Receive :: Client ref e m -> Action IN ref e m
    Ignore  :: Action mode ref e m

newtype Stream (io :: Mode) ref e m = 
  Stream 
    { stream :: [Action io ref e m] 
    } 

initStream :: Action mode ref e m -> Stream mode ref e m 
initStream x = Stream [x]

concatStream :: [Stream mode ref e m ] -> Stream mode ref e m 
concatStream xs = Stream (concatMap stream xs)

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

disconnectAction :: Action i ref e m -> Action OUT ref e m 
disconnectAction (Receive Client { clientID })  = Update $ delete clientID
disconnectAction _ = Ignore

disconnect :: Stream mode ref e m -> Stream OUT ref e m
disconnect Stream { stream } = Stream (map disconnectAction stream)

toResponseStream 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      , RunAction ref m
      ) 
  => (  GQLRequest
     -> ResponseStream e m (Value VALID)
     )
  ->  Action IN ref e m 
  -> m (Action OUT ref e m)
toResponseStream app ref@(Receive client)
  = runInput ref 
    >>= apolloToAction 
          app
          client
          . apolloFormat
toResponseStream _ (Update x) = pure (Update x) 
toResponseStream _ _ = pure Ignore

mapS 
  :: (Monad m)
  => ( Action mode ref e m 
     -> m (Action mode' ref e m)
     )
  -> Stream mode ref e m 
  -> m (Stream mode' ref e m)
mapS f Stream { stream  } = do
    stream' <- traverse f stream
    pure $ Stream { stream = stream' }

traverseS 
  :: (Monad m)
  => ( Action mode ref e m 
     -> m (Stream mode' ref e m)
     )
  -> Stream mode ref e m 
  -> m (Stream mode' ref e m)
traverseS f Stream { stream  } 
  = concatStream 
    <$> traverse f stream

-- EXECUTION
notify :: MonadIO m => Notification Connection m -> m ()
notify (Notification connection msg) = msg >>= liftIO . sendTextData connection

readState :: (MonadIO m) => GQLState ref e m -> m (PubSubStore ref e m)
readState = liftIO . readMVar 

modifyState_ :: (MonadIO m) => GQLState ref e m -> (PubSubStore ref e m -> PubSubStore ref e m) -> m ()
modifyState_ state update = liftIO $ modifyMVar_ state (return . update)

class (MonadIO m, Applicative m) => RunAction ref m where
  runAction :: GQLState ref e m -> Action OUT ref e m -> m ()
  runInput :: Action IN ref e m -> m ByteString

instance (MonadIO m, Applicative m) => RunAction Connection m where
  runAction state (Update update)  
    = modifyState_ state update
  runAction state (Notify toNotification)  
    = readState state 
      >>= traverse_ notify  
        . toNotification
  runAction _ (Error x) = liftIO (print x)
  runAction _ Ignore = pure ()
  runInput (Receive Client { clientConnection }) 
    = liftIO (receiveData clientConnection)

runStream :: (RunAction ref m) => Stream OUT ref e m -> GQLState ref e m ->  m ()
runStream Stream { stream } state = traverse_ (runAction state) stream