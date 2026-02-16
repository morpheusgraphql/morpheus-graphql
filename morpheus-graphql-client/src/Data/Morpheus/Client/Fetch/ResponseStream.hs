{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.ResponseStream
  ( request,
    forEach,
    single,
    ResponseStream,
    GQLClient,
    withHeaders,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Morpheus.Client.Fetch (Args)
import Data.Morpheus.Client.Fetch.GQLClient
import Data.Morpheus.Client.Fetch.Http (httpRequest)
import Data.Morpheus.Client.Fetch.RequestType
  ( ClientTypeConstraint,
    Request (..),
    isSubscription,
  )
import Data.Morpheus.Client.Fetch.Types
import Data.Morpheus.Client.Fetch.WebSockets
  ( endSession,
    receiveResponse,
    responseStream,
    sendInitialRequest,
    sendRequest,
    useWS,
  )
import qualified Data.Text as T
import Relude hiding (ByteString)
import Text.URI (URI, mkURI)

parseURI :: (MonadFail m) => String -> m URI
parseURI url = maybe (fail ("Invalid Endpoint: " <> show url <> "!")) pure (mkURI (T.pack url))

requestSingle :: ResponseStream a -> IO (Either (FetchError a) a)
requestSingle ResponseStream {..}
  | isSubscription _req = useWS _uri _headers wsApp
  | otherwise = httpRequest _uri _req _headers
  where
    wsApp conn = do
      let sid = "0243134"
      sendInitialRequest conn
      sendRequest conn sid _req
      x <- receiveResponse conn
      endSession conn sid
      pure x

requestMany :: (MonadIO m, MonadUnliftIO m, MonadFail m) => (GQLClientResult a -> m ()) -> ResponseStream a -> m ()
requestMany f ResponseStream {..}
  | isSubscription _req = useWS _uri _headers appWS
  | otherwise = liftIO (httpRequest _uri _req _headers) >>= f
  where
    appWS conn = do
      let sid = "0243134"
      sendInitialRequest conn
      sendRequest conn sid _req
      traverse_ (>>= f) (responseStream conn)
      endSession conn sid

-- PUBLIC API
data ResponseStream a
  = (ClientTypeConstraint a) =>
  ResponseStream
  { _req :: Request a,
    _uri :: URI,
    _headers :: Headers
    -- _wsConnection :: Connection
  }

request :: (ClientTypeConstraint a, MonadFail m) => GQLClient -> Args a -> m (ResponseStream a)
request GQLClient {clientURI, clientHeaders} requestArgs = do
  _uri <- parseURI clientURI
  let _req = Request {requestArgs}
  pure ResponseStream {_req, _uri, _headers = clientHeaders}

-- | returns first response from the server
single :: (MonadIO m) => ResponseStream a -> m (GQLClientResult a)
single = liftIO . requestSingle

-- | returns loop listening subscription events forever. if you want to run it in background use `forkIO`
forEach :: (MonadIO m, MonadUnliftIO m, MonadFail m) => (GQLClientResult a -> m ()) -> ResponseStream a -> m ()
forEach = requestMany
