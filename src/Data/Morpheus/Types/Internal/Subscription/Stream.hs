{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Types.Internal.Subscription.Stream
  ( toOutStream,
    runStreamWS,
    runStreamHTTP,
    Stream,
    Scope (..),
    Input (..),
    API (..),
    HTTP,
    WS,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (traverse_)
-- MORPHEUS
import Data.Morpheus.Error
  ( globalErrorMessage,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Message,
    VALID,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Operation
  ( failure,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( GQLChannel (..),
    ResponseEvent (..),
    ResponseStream,
    Result (..),
    ResultT (..),
    runResultT,
  )
import Data.Morpheus.Types.Internal.Subscription.Apollo
  ( ApolloAction (..),
    apolloFormat,
    toApolloResponse,
  )
import Data.Morpheus.Types.Internal.Subscription.ClientConnectionStore
  ( ClientConnectionStore,
    ID,
    Session,
    Updates (..),
    endSession,
    insert,
    startSession,
  )

data API = HTTP | WS

type WS = 'WS

type HTTP = 'HTTP

data
  Input
    (api :: API)
  where
  Init :: ID -> Input WS
  Request :: GQLRequest -> Input HTTP

run :: Scope WS e m -> Updates e m -> m ()
run ScopeWS {update} (Updates changes) = update changes

data Scope (api :: API) event (m :: * -> *) where
  ScopeHTTP ::
    { httpCallback :: event -> m ()
    } ->
    Scope HTTP event m
  ScopeWS ::
    { listener :: m ByteString,
      callback :: ByteString -> m (),
      update :: (ClientConnectionStore event m -> ClientConnectionStore event m) -> m ()
    } ->
    Scope WS event m

data
  Stream
    (api :: API)
    e
    (m :: * -> *)
  where
  StreamWS ::
    { streamWS :: Scope WS e m -> m (Either ByteString [Updates e m])
    } ->
    Stream WS e m
  StreamHTTP ::
    { streamHTTP :: Scope HTTP e m -> m GQLResponse
    } ->
    Stream HTTP e m

handleResponseStream ::
  ( Eq (StreamChannel e),
    GQLChannel e,
    Monad m
  ) =>
  Session ->
  ResponseStream e m (Value VALID) ->
  Stream WS e m
handleResponseStream session (ResultT res) =
  StreamWS $ const $ unfoldR <$> res
  where
    execute Publish {} = apolloError $ globalErrorMessage "websocket can only handle subscriptions, not mutations"
    execute (Subscribe sub) = Right $ startSession sub session
    --------------------------
    unfoldR Success {events} = traverse execute events
    unfoldR Failure {errors} = apolloError errors
    --------------------------
    apolloError :: GQLErrors -> Either ByteString a
    apolloError = Left . toApolloResponse (snd session) . Errors

handleWSRequest ::
  ( Monad m,
    Eq (StreamChannel e),
    GQLChannel e,
    Functor m
  ) =>
  ( GQLRequest ->
    ResponseStream e m (Value VALID)
  ) ->
  ID ->
  ByteString ->
  Stream WS e m
handleWSRequest gqlApp clientId = handle . apolloFormat
  where
    --handle :: Applicative m => Validation ApolloAction -> Stream WS e m
    handle = either (liftWS . Left) handleAction
    --------------------------------------------------
    -- handleAction :: ApolloAction -> Stream WS e m
    handleAction ConnectionInit = liftWS $ Right []
    handleAction (SessionStart sessionId request) =
      handleResponseStream (clientId, sessionId) (gqlApp request)
    handleAction (SessionStop sessionId) =
      liftWS $
        Right [endSession (clientId, sessionId)]

liftWS ::
  Applicative m =>
  Either ByteString [Updates e m] ->
  Stream WS e m
liftWS = StreamWS . const . pure

runStreamWS ::
  (Monad m) =>
  Scope WS e m ->
  Stream WS e m ->
  m ()
runStreamWS scope@ScopeWS {callback} StreamWS {streamWS} =
  streamWS scope
    >>= either callback (traverse_ (run scope))

runStreamHTTP ::
  (Monad m) =>
  Scope HTTP e m ->
  Stream HTTP e m ->
  m GQLResponse
runStreamHTTP scope StreamHTTP {streamHTTP} =
  streamHTTP scope

toOutStream ::
  ( Monad m,
    Eq (StreamChannel e),
    GQLChannel e,
    Functor m
  ) =>
  ( GQLRequest ->
    ResponseStream e m (Value VALID)
  ) ->
  Input api ->
  Stream api e m
toOutStream app (Init clienId) =
  StreamWS handle
  where
    handle ws@ScopeWS {listener, callback} = do
      let runS (StreamWS x) = x ws
      bla <- listener >>= runS . handleWSRequest app clienId
      pure $ (Updates (insert clienId callback) :) <$> bla
toOutStream app (Request req) = StreamHTTP $ handleResponseHTTP (app req)

handleResponseHTTP ::
  ( Eq (StreamChannel e),
    GQLChannel e,
    Monad m
  ) =>
  ResponseStream e m (Value VALID) ->
  Scope HTTP e m ->
  m GQLResponse
handleResponseHTTP
  res
  ScopeHTTP {httpCallback} = do
    x <- runResultT (handleRes res execute)
    case x of
      Success r _ events -> do
        traverse_ httpCallback events
        pure $ Data r
      Failure err -> pure (Errors err)
    where
      execute (Publish event) = pure event
      execute Subscribe {} = failure ("http can't handle subscription" :: Message)

handleRes ::
  ( Eq (StreamChannel e),
    GQLChannel e,
    Monad m
  ) =>
  ResponseStream e m a ->
  (ResponseEvent e m -> ResultT e' m e') ->
  ResultT e' m a
handleRes res execute = ResultT $ runResultT res >>= runResultT . unfoldRes execute

unfoldRes ::
  (Monad m) =>
  (e -> ResultT e' m e') ->
  Result e a ->
  ResultT e' m a
unfoldRes execute Success {events, result, warnings} = do
  events' <- traverse execute events
  ResultT $ pure $
    Success
      { result,
        warnings,
        events = events'
      }
unfoldRes _ Failure {errors} = ResultT $ pure $ Failure {errors}
