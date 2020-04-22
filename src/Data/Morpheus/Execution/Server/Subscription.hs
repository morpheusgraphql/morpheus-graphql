{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE OverloadedStrings       #-}

module Data.Morpheus.Execution.Server.Subscription
  ( Client
  , connect
  , disconnect
  , toOutStream
  , runStreamWS
  , runStreamHTTP
  , Stream
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
import           Data.Morpheus.Error.Utils      ( globalErrorMessage
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Value(..)
                                                , VALID
                                                , GQLErrors
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..) 
                                                , GQLResponse(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( failure )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( SubAction(..)
                                                , apolloFormat
                                                , toApolloResponse
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
                                                )
 


connect :: IO (Input 'Ws)
connect = Init <$> nextRandom

disconnect:: Scope 'Ws e m -> Input 'Ws -> m ()
disconnect WS { update }  (Init clientID)  = update (delete clientID)

updateClient
  :: (Client e m -> Client e m ) 
  -> ID
  -> Updates e m 
updateClient  f cid = Updates (adjust f cid)

endSession :: Session -> Updates e m 
endSession (clientId, sessionId) = updateClient endSub clientId
 where
  endSub client = client { clientSessions = HM.delete sessionId (clientSessions client) }

startSession :: SubEvent e m -> Session -> Updates e m 
startSession  subscriptions (clientId, sessionId) = updateClient startSub clientId
 where
  startSub client = client { clientSessions = HM.insert sessionId subscriptions (clientSessions client) }

type Session = (ID, SesionID)

data Input 
  (api:: API) 
  where
  Init :: ID -> Input 'Ws 
  Request :: GQLRequest -> Input 'Http 

newtype Updates e ( m :: * -> * ) =  
    Updates {
      _runUpdate:: PubSubStore e m -> PubSubStore e m  
    }

run :: Scope 'Ws e m -> Updates e m -> m ()
run WS { update } (Updates changes) = update changes

data Scope (api :: API ) event (m :: * -> * ) where
  HTTP :: {
      httpCallback :: event -> m ()
    } -> Scope 'Http event m
  WS :: 
     { listener :: m ByteString
     , callback :: ByteString -> m ()
     , update   :: (PubSubStore event m -> PubSubStore event m) -> m ()
    } -> Scope 'Ws event m

data API = Http | Ws

data Stream 
    (api:: API) 
    e 
    (m :: * -> * ) 
  where
  StreamWS 
    :: 
    { streamWS ::  Scope 'Ws e m -> m (Either ByteString [Updates e m])
    } -> Stream 'Ws e m
  StreamHTTP
    :: 
    { streamHTTP :: Scope 'Http e m -> m GQLResponse
    } -> Stream 'Http e m

handleResponseStream
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => Session
  -> ResponseStream e m (Value VALID)
  -> Stream 'Ws e m 
handleResponseStream session (ResultT res) 
  = StreamWS $ const $ unfoldR <$> res  
    where
      execute Publish   {} = apolloError $ globalErrorMessage "websocket can only handle subscriptions, not mutations"
      execute (Subscribe sub) = Right $ startSession sub session
      -------------------
      unfoldR Success { events } = traverse execute events
      unfoldR Failure { errors } = apolloError errors
      ---------------
      apolloError :: GQLErrors -> Either ByteString a
      apolloError = Left . toApolloResponse (snd session) . Errors  

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
    handleApollo (SubError err) 
      = StreamWS $ const $ -- bla
        pure $ Right []
    handleApollo (AddSub sessionId request) 
      = handleResponseStream (clientId, sessionId) (gqlApp request) 
    handleApollo (RemoveSub sessionId)
      = StreamWS $ const $ pure $ Right [endSession (clientId, sessionId)]


runStreamWS 
  :: (Monad m) 
  => Scope 'Ws e m
  -> Stream 'Ws e m 
  ->  m ()
runStreamWS scope@WS{ callback } StreamWS { streamWS }  
  = streamWS scope 
    >>= either callback (traverse_ (run scope))

runStreamHTTP
  :: (Monad m) 
  => Scope 'Http e m
  -> Stream 'Http e m 
  ->  m GQLResponse
runStreamHTTP scope StreamHTTP { streamHTTP }  
  = streamHTTP scope

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
          let runS (StreamWS x) = x ws
          bla <- listener >>= runS . handleWSRequest app clienId
          pure $ (Updates (insert clienId callback) :) <$> bla
toOutStream app (Request req) = StreamHTTP $ handleResponseHTTP (app req)

handleResponseHTTP
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => ResponseStream e m (Value VALID)
  -> Scope 'Http e m
  -> m GQLResponse 
handleResponseHTTP 
  res
  HTTP { httpCallback } = do
    x <- runResultT (handleRes res execute)
    case x of 
      Success r _ events-> do 
        traverse_ httpCallback events
        pure $ Data r 
      Failure err -> pure (Errors err)
    where
     execute (Publish event) = pure event
     execute Subscribe {}  = failure ("http can't handle subscription" :: String)

handleRes
  ::  (  Eq (StreamChannel e)
      , GQLChannel e
      , Monad m
      )
  => ResponseStream e m a
  -> (ResponseEvent e m -> ResultT e' m e')
  -> ResultT e' m a
handleRes res execute = ResultT $ runResultT res >>= runResultT . unfoldRes execute

unfoldRes 
  :: (Monad m) 
    => (e -> ResultT e' m e')
  -> Result e a
  ->  ResultT e' m a
unfoldRes execute Success { events, result, warnings } = do
  events' <- traverse execute events
  ResultT $ pure $ Success 
    { result
    , warnings
    , events = events'
    }
unfoldRes _ Failure { errors } = ResultT $ pure $ Failure { errors }