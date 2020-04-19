{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE ScopedTypeVariables     #-}


module Data.Morpheus.Execution.Server.Subscription
  ( Client
  , connect
  , disconnect
  , publishEvents
  , Stream(..)
  , toOutStream
  , handleResponseStream
  , IN
  , OUT
  , traverseS
  , PubSubStore
  , Action(..)
  , Scope(..)
  )
where

import           Data.Foldable                  ( traverse_ )
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
                                                , elems
                                                , insert
                                                , adjust
                                                , delete
                                                , ID
                                                )
 

connect :: Monad m => IO (Stream IN client e m)
connect = singleton . Init <$> nextRandom

updateClient
  :: (Client e m -> Client e m ) 
  -> ID
  -> Action OUT ref e m 
updateClient  f cid = Update (adjust f cid)

publishEvents
  :: ( Eq (StreamChannel e)
     , Monad m 
     , GQLChannel e
     ) 
  => e 
  -> Stream OUT ref e m 
publishEvents = singleton . publishEvent

publishEvent
  :: ( Eq (StreamChannel e)
     , Monad m 
     , GQLChannel e
     ) 
  => e 
  -> Action OUT ref e m 
publishEvent event = Notify $ traverse_ sendMessage . elems
 where
  sendMessage Client { clientSessions, clientCallback }
    | null clientSessions  = pure ()
    | otherwise = traverse_ send (filterByChannels clientSessions)
   where
    send (sid, Event { content = subscriptionRes }) 
      = toApolloResponse sid <$> subscriptionRes event >>= clientCallback
    ---------------------------
    filterByChannels = filter
      ( not
      . null
      . intersect (streamChannels event)
      . channels
      . snd
      ) . HM.toList

endSession :: Session -> Action OUT ref e m 
endSession (clientId, sessionId) = updateClient endSub clientId
 where
  endSub client = client { clientSessions = HM.delete sessionId (clientSessions client) }

startSession :: SubEvent e m -> SesionID -> ID -> Action OUT ref e m 
startSession  subscriptions sid = updateClient startSub
 where
  startSub client = client { clientSessions = HM.insert sid subscriptions (clientSessions client) }

data Mode = In | Out

type IN = 'In 
type OUT = 'Out 
type Session = (ID, Name)

data Action 
    (mode :: Mode)
    ref 
    e 
    (m :: * -> * )
  where 
    Init     :: ID -> Action IN ref e m
    -- Request  :: GQLRequest -> Action IN ref e m
    -------------------------------------------
    Response :: GQLResponse -> Action OUT ref e m 
    Update   :: (PubSubStore e m -> PubSubStore e m) -> Action OUT ref e m 
    Notify   :: (PubSubStore e m -> m ()) -> Action OUT ref e m
    Error    :: String -> Action OUT ref e m

data Scope m
  = HTTP 
   | WS 
     { listener :: m ByteString
     , callback :: ByteString -> m ()
     }

newtype Stream (io :: Mode) ref e m = 
  Stream 
    { stream 
        :: m ()  -- ignore 
        -> Scope m  -- scope
        -> m [Action io ref e m] 
    }

instance 
  Applicative m 
  => Empty (Stream t ref e m) where
  empty = Stream $ const $ const $ pure []

singleton :: Applicative m => Action mode ref e m -> Stream mode ref e m 
singleton x =  Stream $ const $ const $ pure [x]

disconnect 
  :: Functor m 
  => Stream mode ref e m 
  -> Stream OUT ref e m
disconnect (Stream x) = Stream $ \r cb -> concatMap __disconnect <$> x r cb
  where
    __disconnect:: Action mode ref e m -> [Action OUT ref e m]
    __disconnect (Init clientID)  = [Update (delete clientID)]
    __disconnect _ = []

handleResponseStream
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => Session
  -> ResponseStream e m (Value VALID)
  -> Stream OUT ref e m
handleResponseStream (clientId, sessionId) resStream 
  = Stream handle
    where
    -- httpServer can't start subscription 
     execute HTTP Subscribe {}  = Error "http can't handle subscription"
     execute _ (Subscribe sub) = startSession sub sessionId clientId
     execute _ (Publish   pub) = publishEvent pub
     --------------------------------------------------------------
     handle _ HTTP = unfoldRes <$> runResultT resStream 
      where
        unfoldRes Success { events } = map (execute HTTP) events
        unfoldRes Failure { errors } = [Response (Errors errors)]
     handle _ ws@WS { callback } = unfoldRes <$> runResultT resStream 
      where
        unfoldRes Success { events } = map (execute ws) events
        unfoldRes Failure { errors } = [notifyError errors]
        --------------------------------------------------------------
        notifyError errors = Notify 
                    $ const
                    $ callback 
                    $ toApolloResponse sessionId 
                    $ Errors errors 

handleWSRequest 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
        -> ResponseStream e m (Value VALID)
     )
  -> ID
  -> ByteString
  -> Stream OUT ref e m
handleWSRequest gqlApp clientId = handleApollo . apolloFormat
  where 
    handleApollo (SubError x) = singleton (Error x)
    handleApollo (AddSub sessionId request) 
      = handleResponseStream (clientId, sessionId) (gqlApp request) 
    handleApollo (RemoveSub sessionId)
      = singleton $ endSession (clientId, sessionId)

toOutStream 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
     -> ResponseStream e m (Value VALID)
     )
  -> Action IN ref e m 
  -> Stream OUT ref e m
toOutStream app (Init clienId) 
  = Stream handle 
      where
        handle ls ws@WS { listener , callback } = do
          (Stream stream) <- handleWSRequest app clienId  <$> listener
          (Update (insert clienId callback) :) <$> stream ls ws
        -- HTTP Server does not have to wait for subsciprions
        handle _ HTTP = pure [] 

traverseS 
  :: (Monad m)
  => ( Action t ref e m 
     -> Stream t' ref e m
     )
  -> Stream t ref e m 
  -> Stream t' ref e m
traverseS f (Stream stream) 
  = Stream handle
      where
        handle r cb 
          = stream r cb 
            >>= fmap concat 
              . traverse (go .f) 
          where 
            go (Stream s) = s  r cb 