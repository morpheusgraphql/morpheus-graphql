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
  , API(..)
  )
where

import           Data.Foldable                  ( traverse_ )
import           Data.ByteString.Lazy.Char8     (ByteString)
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.HashMap.Lazy   as   HM    ( insert
                                                , delete
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST
                                                ( Value(..)
                                                , VALID
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..) 
                                                , GQLResponse
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( failure )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( SubAction(..)
                                                , apolloFormat
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( SubEvent
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
                                                , insert
                                                , adjust
                                                , delete
                                                , ID
                                                , publish
                                                )
 

connect :: IO (Input 'Ws)
connect = Init <$> nextRandom

disconnect:: Input 'Ws -> Action e m
disconnect (Init clientID)  = Update (delete clientID)

updateClient
  :: (Client e m -> Client e m ) 
  -> ID
  -> Action e m 
updateClient  f cid = Update (adjust f cid)

endSession :: Session -> Action e m 
endSession (clientId, sessionId) = updateClient endSub clientId
 where
  endSub client = client { clientSessions = HM.delete sessionId (clientSessions client) }

startSession :: SubEvent e m -> Session -> Action e m 
startSession  subscriptions (clientId, sessionId) = updateClient startSub clientId
 where
  startSub client = client { clientSessions = HM.insert sessionId subscriptions (clientSessions client) }

type Session = (ID, SesionID)

data Input 
  (api:: API) 
  where
  Init :: ID -> Input 'Ws 
  Request :: GQLRequest -> Input 'Http 

data Action
    e 
    (m :: * -> * )
  where 
    Update   :: (PubSubStore e m -> PubSubStore e m) -> Action e m 

data Scope (api :: API ) event (m :: * -> * ) where
  HTTP :: {
      httpCallback :: event -> m ()
    } -> Scope 'Http event m
  WS :: 
     { listener :: m ByteString
     , callback :: ByteString -> m ()
    } -> Scope 'Ws event m

data API = Http | Ws

data Stream 
    (api:: API) 
    e 
    (m :: * -> * ) 
  where
  StreamWS 
    :: 
    { streamWS ::  Scope 'Ws e m -> ResultT (Action e m)  m (Value VALID)
    } -> Stream 'Ws e m
  StreamHTTP
    :: 
    { streamHTTP :: (e -> m()) -> ResultT e m (Value VALID)
    } -> Stream 'Http e m

handleResponseStream
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => Session
  -> ResponseStream e m (Value VALID)
  -> Stream 'Ws e m 
handleResponseStream session res 
  = StreamWS handle
    where
    -- httpServer can't start subscription 
     execute WS{}   Publish   {} = failure "websocket can only handle subscriptions, not mutations"
     execute WS{}   (Subscribe sub) = pure $ startSession sub session
     --------------------------------------------------------------
     handle ws = ResultT $ runResultT res >>= runResultT . unfoldRes
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
  -> Stream 'Ws e m
handleWSRequest gqlApp clientId = handleApollo . apolloFormat
  where 
    handleApollo (SubError x) = StreamWS $ const $ failure x
    handleApollo (AddSub sessionId request) 
      = handleResponseStream (clientId, sessionId) (gqlApp request) 
    handleApollo (RemoveSub sessionId)
      = StreamWS $ const $ ResultT $ pure $ 
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
  -> Input api
  -> Stream api e m
toOutStream app (Init clienId) 
  = StreamWS handle 
      where
        handle ws@WS { listener , callback } = do
          let withUpdate x = Success x [] [Update (insert clienId callback)] 
          let runS (StreamWS x) = x ws
          ResultT (withUpdate <$> listener) >>= runS . handleWSRequest app clienId
toOutStream app (Request req) = handleResponseHTTP (app req)

handleResponseHTTP
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => ResponseStream e m (Value VALID)
  -> Stream 'Http e m 
handleResponseHTTP res 
  = StreamHTTP handle
    where
     execute (Publish event) = pure event
     execute Subscribe {}  = failure "http can't handle subscription"
     --------------------------------------------------------------
     handle _ = ResultT $ runResultT res >>= runResultT . unfoldRes
      where
        unfoldRes Success { events, result, warnings } = do
          events' <- traverse execute events
          ResultT $ pure $ Success 
            { result
            , warnings
            , events = events'
            }
        unfoldRes Failure { errors } = ResultT $ pure $ Failure { errors }
