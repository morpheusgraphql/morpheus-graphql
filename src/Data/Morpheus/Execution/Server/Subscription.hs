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
  , Stream(..)
  , toOutStream
  , handleResponseStream
  , PubSubStore
  , Action(..)
  , Scope(..)
  , Input(..)
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
                                                ( Value(..)
                                                , VALID
                                                )
import           Data.Morpheus.Types.IO         ( GQLResponse(..)
                                                , GQLRequest(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( failure )
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
                                                , ResultT(..)
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
 

connect :: IO Input
connect = Init <$> nextRandom

disconnect:: Input -> [Action ref e m]
disconnect (Init clientID)  = [Update (delete clientID)]
disconnect _ = []

updateClient
  :: (Client e m -> Client e m ) 
  -> ID
  -> Action ref e m 
updateClient  f cid = Update (adjust f cid)

publishEvent
  :: ( Eq (StreamChannel e)
     , Monad m 
     , GQLChannel e
     ) 
  => e 
  -> Action ref e m 
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

endSession :: Session -> Action ref e m 
endSession (clientId, sessionId) = updateClient endSub clientId
 where
  endSub client = client { clientSessions = HM.delete sessionId (clientSessions client) }

startSession :: SubEvent e m -> Session -> Action ref e m 
startSession  subscriptions (clientId, sessionId) = updateClient startSub clientId
 where
  startSub client = client { clientSessions = HM.insert sessionId subscriptions (clientSessions client) }

type Session = (ID, SesionID)

data Input 
  = Init ID
  | Request GQLRequest 

data Action
    ref 
    e 
    (m :: * -> * )
  where 
    Update   :: (PubSubStore e m -> PubSubStore e m) -> Action ref e m 
    Notify   :: (PubSubStore e m -> m ()) -> Action ref e m

data Scope m
  = HTTP 
   | WS 
     { listener :: m ByteString
     , callback :: ByteString -> m ()
     }

newtype Stream ref e m = 
  Stream 
    { stream 
        :: m ()  -- ignore 
        -> Scope m  -- scope
        -> ResultT (Action ref e m)  m (Value VALID)
    }

handleResponseStream
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => Session
  -> ResponseStream e m (Value VALID)
  -> Stream ref e m
handleResponseStream session res 
  = Stream handle
    where
    -- httpServer can't start subscription 
     execute HTTP Subscribe {}  = failure "http can't handle subscription"
     execute _ (Subscribe sub) = pure $ startSession sub session
     execute _ (Publish   pub) = pure $ publishEvent pub
     --------------------------------------------------------------
     handle _ ws = ResultT $ runResultT res >>= runResultT . unfoldRes
      where
        unfoldRes Success { events, result, warnings } = do
          events' <- traverse (execute ws) events
          ResultT $ pure $ Success 
            { result
            , warnings
            , events = events'
            }
        unfoldRes Failure { errors } = ResultT $ pure $ Failure { errors }
        --------------------------------------------------------------
        -- TODO:
        -- notifyError errors = Notify 
        --             $ const
        --             $ callback 
        --             $ toApolloResponse sessionId 
        --             $ Errors errors 

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
  -> Stream ref e m
handleWSRequest gqlApp clientId = handleApollo . apolloFormat
  where 
    handleApollo (SubError x) = Stream $ const $ const $ failure x
    handleApollo (AddSub sessionId request) 
      = handleResponseStream (clientId, sessionId) (gqlApp request) 
    handleApollo (RemoveSub sessionId)
      = Stream $ const $ const $ ResultT $ pure $ 
        Success Null [] [endSession (clientId, sessionId)]

toOutStream 
  ::  ( Monad m
      , Eq (StreamChannel e)
      , GQLChannel e
      , Functor m
      ) 
  => (  GQLRequest
     -> ResponseStream e m (Value VALID)
     )
  -> Input 
  -> Stream ref e m
toOutStream app (Init clienId) 
  = Stream handle 
      where
        handle ls ws@WS { listener , callback } = do
          let withUpdate x = Success x [] [Update (insert clienId callback)] 
          let runS (Stream x) = x ls ws
          ResultT (withUpdate <$> listener) >>= runS . handleWSRequest app clienId 
        -- HTTP Server does not have to wait for subsciprions
        handle _ HTTP = failure "ws in hhtp are not allowed"
toOutStream app (Request req) = handleResponseStream undefined (app req)