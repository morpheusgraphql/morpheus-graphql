{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Subscriptions.Stream
  ( toOutStream,
    runStreamWS,
    runStreamHTTP,
    ApiContext (..),
    Input (..),
    Output,
    API (..),
    PUB,
    SUB,
  )
where

import Control.Monad.Except (throwError)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.App.Internal.Resolving
  ( Channel,
    ResponseEvent (..),
    ResponseStream,
    Result (..),
    ResultT (..),
    runResultT,
  )
import Data.Morpheus.Subscriptions.Apollo
  ( ApolloAction (..),
    ApolloMessageType (..),
    apolloFormat,
    toApolloResponse,
  )
import Data.Morpheus.Subscriptions.ClientConnectionStore
  ( ClientConnectionStore,
    SessionID (..),
    Updates (..),
    endSession,
    insertConnection,
    startSession,
  )
import Data.Morpheus.Subscriptions.Event (Event (..))
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    VALID,
    Value (..),
  )
import Data.UUID (UUID)
import Relude hiding (ByteString)

data API = PUB | SUB

type SUB = 'SUB

type PUB = 'PUB

data
  Input
    (api :: API)
  where
  InitConnection :: UUID -> Input SUB
  Request :: GQLRequest -> Input PUB

run :: ApiContext SUB e m -> Updates e m -> m ()
run SubContext {updateStore} (Updates changes) = updateStore changes

data ApiContext (api :: API) event (m :: Type -> Type) where
  PubContext ::
    { eventPublisher :: event -> m ()
    } ->
    ApiContext PUB event m
  SubContext ::
    { listener :: m ByteString,
      callback :: ByteString -> m (),
      updateStore :: (ClientConnectionStore event m -> ClientConnectionStore event m) -> m ()
    } ->
    ApiContext SUB event m

data WSOutputEvent e m
  = WSUpdate (Updates e m)
  | WSMessage ByteString

data
  Output
    (api :: API)
    (e :: Type)
    (m :: Type -> Type)
  where
  SubOutput ::
    { streamWS :: ApiContext SUB e m -> m (Either ByteString [WSOutputEvent e m])
    } ->
    Output SUB e m
  PubOutput ::
    { streamHTTP :: ApiContext PUB e m -> m GQLResponse
    } ->
    Output PUB e m

handleResponseStream ::
  ( Monad m,
    Eq (Channel e),
    Hashable (Channel e)
  ) =>
  SessionID ->
  ResponseStream e m (Value VALID) ->
  Output SUB e m
handleResponseStream session (ResultT res) =
  SubOutput $ const $ unfoldR <$> res
  where
    execute Publish {} =
      apolloError
        ["websocket can only handle subscriptions, not mutations"]
    execute (Subscribe ch subRes) =
      Right . WSUpdate $ startSession ch subRes session
    --------------------------
    unfoldR Success {result = (events, _)} =
      traverse execute events
    unfoldR Failure {errors} =
      apolloError (toList errors)
    --------------------------
    apolloError :: [GQLError] -> Either ByteString a
    apolloError = Left . toApolloResponse GqlError (Just $ sid session) . Just . Errors

handleWSRequest ::
  ( Monad m,
    Functor m,
    Eq ch,
    Hashable ch
  ) =>
  ( GQLRequest ->
    ResponseStream (Event ch con) m (Value VALID)
  ) ->
  UUID ->
  ByteString ->
  Output SUB (Event ch con) m
handleWSRequest gqlApp clientId = handle . apolloFormat
  where
    -- handle :: Applicative m => Validation ApolloAction -> Stream SUB e m
    handle = either (liftWS . Left) handleAction
    --------------------------------------------------
    -- handleAction :: ApolloAction -> Stream SUB e m
    handleAction ConnectionInit = do
      liftWS $ Right [WSMessage $ toApolloResponse GqlConnectionAck Nothing Nothing]
    handleAction Ping = do
      liftWS $ Right [WSMessage $ toApolloResponse GqlPong Nothing Nothing]
    handleAction (SessionStart sessionId request) =
      handleResponseStream (SessionID clientId sessionId) (gqlApp request)
    handleAction (SessionStop sessionId) =
      liftWS
        $ Right [WSUpdate $ endSession (SessionID clientId sessionId)]

liftWS ::
  (Applicative m) =>
  Either ByteString [WSOutputEvent e m] ->
  Output SUB e m
liftWS = SubOutput . const . pure

runStreamWS ::
  (Monad m) =>
  ApiContext SUB e m ->
  Output SUB e m ->
  m ()
runStreamWS scope@SubContext {callback} SubOutput {streamWS} =
  streamWS scope
    >>= either callback (traverse_ eventRunner)
  where
    -- eventRunner :: Monad m => WSOutputEvent e m -> m ()
    eventRunner (WSUpdate updates) = run scope updates
    eventRunner (WSMessage msg) = callback msg

runStreamHTTP ::
  (Monad m) =>
  ApiContext PUB e m ->
  Output PUB e m ->
  m GQLResponse
runStreamHTTP scope PubOutput {streamHTTP} =
  streamHTTP scope

toOutStream ::
  ( Monad m,
    Eq ch,
    Hashable ch
  ) =>
  ( GQLRequest ->
    ResponseStream (Event ch con) m (Value VALID)
  ) ->
  Input api ->
  Output api (Event ch con) m
toOutStream app (InitConnection clientId) =
  SubOutput handle
  where
    handle ws@SubContext {listener, callback} = do
      let runS (SubOutput x) = x ws
      bla <- listener >>= runS . handleWSRequest app clientId
      pure $ ((WSUpdate $ Updates (insertConnection clientId callback)) :) <$> bla
toOutStream app (Request req) =
  PubOutput $ handleResponseHTTP (app req)

handleResponseHTTP ::
  (Monad m) =>
  ResponseStream e m (Value VALID) ->
  ApiContext PUB e m ->
  m GQLResponse
handleResponseHTTP
  res
  PubContext {eventPublisher} = runResultT (handleRes res execute) >>= runResult
    where
      runResult Success {result = (events, result)} = traverse_ eventPublisher events $> Data result
      runResult Failure {errors} = pure $ Errors $ toList errors
      execute (Publish event) = pure event
      execute Subscribe {} = throwError "http server can't handle subscription"

handleRes ::
  (Monad m) =>
  ResponseStream e m a ->
  (ResponseEvent e m -> ResultT e' m e') ->
  ResultT e' m a
handleRes res execute = ResultT $ runResultT res >>= runResultT . unfoldRes execute

unfoldRes ::
  (Monad m) =>
  (e -> ResultT e' m e') ->
  Result GQLError ([e], a) ->
  ResultT e' m a
unfoldRes execute Success {result = (events, result), ..} = traverse execute events >>= packResultT
  where
    packResultT events' = ResultT $ pure $ Success {result = (events', result), ..}
unfoldRes _ Failure {errors} = ResultT $ pure $ Failure {errors}
