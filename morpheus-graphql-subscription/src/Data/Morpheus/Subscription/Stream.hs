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

module Data.Morpheus.Subscription.Stream
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

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Error
  ( globalErrorMessage,
  )
import Data.Morpheus.Internal.Utils
  ( failure,
  )
import Data.Morpheus.Subscription.Apollo
  ( ApolloAction (..),
    apolloFormat,
    toApolloResponse,
  )
import Data.Morpheus.Subscription.ClientConnectionStore
  ( ClientConnectionStore,
    SessionID (..),
    Updates (..),
    endSession,
    insertConnection,
    startSession,
  )
import Data.Morpheus.Subscription.Event (Event (..))
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    VALID,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Channel,
    ResponseEvent (..),
    ResponseStream,
    Result (..),
    ResultT (..),
    runResultT,
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

data ApiContext (api :: API) event (m :: * -> *) where
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

data
  Output
    (api :: API)
    e
    (m :: * -> *)
  where
  SubOutput ::
    { streamWS :: ApiContext SUB e m -> m (Either ByteString [Updates e m])
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
    execute Publish {} = apolloError $ globalErrorMessage "websocket can only handle subscriptions, not mutations"
    execute (Subscribe ch subRes) = Right $ startSession ch subRes session
    --------------------------
    unfoldR Success {events} = traverse execute events
    unfoldR Failure {errors} = apolloError errors
    --------------------------
    apolloError :: GQLErrors -> Either ByteString a
    apolloError = Left . toApolloResponse (sid session) . Errors

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
    --handle :: Applicative m => Validation ApolloAction -> Stream SUB e m
    handle = either (liftWS . Left) handleAction
    --------------------------------------------------
    -- handleAction :: ApolloAction -> Stream SUB e m
    handleAction ConnectionInit = liftWS $ Right []
    handleAction (SessionStart sessionId request) =
      handleResponseStream (SessionID clientId sessionId) (gqlApp request)
    handleAction (SessionStop sessionId) =
      liftWS $
        Right [endSession (SessionID clientId sessionId)]

liftWS ::
  Applicative m =>
  Either ByteString [Updates e m] ->
  Output SUB e m
liftWS = SubOutput . const . pure

runStreamWS ::
  (Monad m) =>
  ApiContext SUB e m ->
  Output SUB e m ->
  m ()
runStreamWS scope@SubContext {callback} SubOutput {streamWS} =
  streamWS scope
    >>= either callback (traverse_ (run scope))

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
      pure $ (Updates (insertConnection clientId callback) :) <$> bla
toOutStream app (Request req) =
  PubOutput $ handleResponseHTTP (app req)

handleResponseHTTP ::
  ( Monad m
  ) =>
  ResponseStream e m (Value VALID) ->
  ApiContext PUB e m ->
  m GQLResponse
handleResponseHTTP
  res
  PubContext {eventPublisher} = runResultT (handleRes res execute) >>= runResult
    where
      runResult Success {result, events} = traverse_ eventPublisher events $> Data result
      runResult Failure {errors} = pure $ Errors errors
      execute (Publish event) = pure event
      execute Subscribe {} = failure (globalErrorMessage "http server can't handle subscription")

handleRes ::
  (Monad m) =>
  ResponseStream e m a ->
  (ResponseEvent e m -> ResultT e' m e') ->
  ResultT e' m a
handleRes res execute = ResultT $ runResultT res >>= runResultT . unfoldRes execute

unfoldRes ::
  (Monad m) =>
  (e -> ResultT e' m e') ->
  Result e a ->
  ResultT e' m a
unfoldRes execute Success {result, warnings, events} =
  traverse execute events
    >>= ( ResultT . pure
            . Success
              result
              warnings
        )
unfoldRes _ Failure {errors} = ResultT $ pure $ Failure {errors}
