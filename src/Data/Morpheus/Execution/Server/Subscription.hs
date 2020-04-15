{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE TypeFamilies            #-}


module Data.Morpheus.Execution.Server.Subscription
  ( Client
  , connect
  , disconnect
  , publishEvents
  , Stream(..)
  , toResponseStream
  , IN
  , OUT
  , traverseS
  , PubSubStore
  , Notification(..)
  , Action(..)
  )
where

import           Data.ByteString.Lazy.Char8     (ByteString)
import           Data.List                      ( intersect )
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.HashMap.Lazy   as   HM    ( toList
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
import           Data.Morpheus.Types.Internal.Operation
                                                ( Empty(..)
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
import           Data.Morpheus.Types.Internal.Subscription
                                                ( Client(..)
                                                , PubSubStore
                                                , SesionID
                                                , concatUnfold
                                                , insert
                                                , adjust
                                                , delete
                                                , ID
                                                )
 

connect :: ref -> IO (Stream IN ref e m)
connect clientConnection = do
  clientID <- nextRandom
  return $ Stream [Init clientID clientConnection ]

updateClient
  :: (Client ref e m -> Client ref e m ) 
  -> ID
  -> Action OUT ref e m 
updateClient  f cid = Update (adjust f cid)

publishEvents
  :: ( Eq (StreamChannel e)
     , Functor m 
     , GQLChannel e
     ) 
  => e 
  -> Stream OUT ref e m 
publishEvents = singleton . publishEvent

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

endSubscription :: SesionID -> ID -> Action OUT ref e m 
endSubscription sid = updateClient endSub
 where
  endSub client = client { clientSessions = HM.delete sid (clientSessions client) }

startSubscription :: SubEvent e m -> SesionID -> ID -> Action OUT ref e m 
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
    Init :: ID -> ref -> Action IN ref e m
    Request :: ref -> (ByteString -> m (Stream OUT ref e m) ) -> Action OUT ref e m 
    Update  :: (PubSubStore ref e m -> PubSubStore ref e m) -> Action OUT ref e m 
    Notify  :: (PubSubStore ref e m -> [Notification ref m]) -> Action OUT ref e m
    Error   :: String -> Action OUT ref e m

newtype Stream (io :: Mode) ref e m = 
  Stream 
    { stream :: [Action io ref e m] 
    }

instance Empty (Stream t ref e m) where
  empty = Stream []

singleton :: Action mode ref e m -> Stream mode ref e m 
singleton x = Stream [x]

disconnect :: Stream mode ref e m -> Stream OUT ref e m
disconnect (Stream x) = Stream $ concatMap __disconnect x
  where
    __disconnect:: Action mode ref e m -> [Action OUT ref e m]
    __disconnect (Init clientID _)  = [Update (delete clientID)]
    __disconnect _ = []

concatStream :: [Stream mode ref e m ] -> Stream mode ref e m 
concatStream xs = Stream (concatMap stream xs)

handleQuery
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => Name
  -> ResponseStream e m (Value VALID)
  -> (ID, ref)
  -> m (Stream OUT ref e m)
handleQuery sessionId resStream (clientId,connection)
  = Stream . unfoldRes <$> runResultT resStream
  where
    unfoldRes Success { events } = map execute events
    unfoldRes Failure { errors } = [notifyError errors]
    --------------------------------------------------------------
    execute (Subscribe sub) = startSubscription sub sessionId clientId 
    execute (Publish   pub) = publishEvent pub
    --------------------------------------------------------------
    notifyError errors = Notify 
                $ const
                [ Notification 
                    connection
                    $ pure 
                    $ toApolloResponse sessionId 
                    $ Errors errors
                ]
 
apolloToAction 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
        -> ResponseStream e m (Value VALID)
     )
  -> (ID,ref)
  -> SubAction
  -> m (Stream OUT ref e m)
apolloToAction _  _ (SubError x) = pure $ singleton (Error x)
apolloToAction gqlApp client (AddSub sessionId request) 
  = handleQuery sessionId (gqlApp request) client
apolloToAction _ (clientId,_) (RemoveSub sessionId)
  = pure $ singleton (endSubscription sessionId clientId)

toResponseStream 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
     -> ResponseStream e m (Value VALID)
     )
  ->  Action IN ref e m 
  -> m (Stream OUT ref e m)
toResponseStream app (Init clienId ref) 
  = pure $ singleton $ Request ref $ \request -> do
      (Stream stream) <- apolloToAction app (clienId,ref) (apolloFormat request)
      pure $ Stream $ Update (insert clienId ref) : stream

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