{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.ResponseStream
  ( request,
    forEach,
    single,
    ResponseStream,
  )
where

import qualified Data.Aeson as A
import Data.Morpheus.Client.Fetch (Args)
import Data.Morpheus.Client.Fetch.Http (post)
import Data.Morpheus.Client.Fetch.RequestType
  ( ClientTypeConstraint,
    Request (..),
    decodeResponse,
    isSubscription,
    toRequest,
  )
import Data.Morpheus.Client.Fetch.WebSockets
  ( responseStream,
    sendInitialRequest,
    sendRequest,
    useWS,
  )
import Data.Morpheus.Client.Internal.Types
import qualified Data.Text as T
import Relude hiding (ByteString)
import Text.URI (URI, mkURI)

parseURI :: MonadFail m => String -> m URI
parseURI url = maybe (fail ("Invalid Endpoint: " <> show url <> "!")) pure (mkURI (T.pack url))

requestSingle :: ClientTypeConstraint a => URI -> Request a -> IO (Either (FetchError a) a)
requestSingle uri r
  | isSubscription r = undefined
  | otherwise = decodeResponse <$> post uri (A.encode $ toRequest r)

requestMany :: ClientTypeConstraint a => URI -> Request a -> (ClientResult a -> IO ()) -> IO ()
requestMany uri r f
  | isSubscription r = useWS uri app
  | otherwise = undefined
  where
    app conn = do
      sendInitialRequest conn
      -- send initial GQL Request for subscription
      sendRequest conn "0243134" r
      -- handle GQL subscription responses
      traverse_ (>>= f) (responseStream conn)

-- PUBLIC API
data ResponseStream a = ClientTypeConstraint a =>
  ResponseStream
  { _req :: Request a,
    _uri :: URI
  }

request :: ClientTypeConstraint a => String -> Args a -> IO (ResponseStream a)
request uri requestArgs = do
  _uri <- parseURI uri
  let _req = Request {requestArgs}
  pure ResponseStream {_req, _uri}

-- returns first response from the server
single :: ResponseStream a -> IO (ClientResult a)
single ResponseStream {_req, _uri} = requestSingle _uri _req

-- returns loop listening subscription events forever. if you want to run it in background use `forkIO`
forEach :: (ClientResult a -> IO ()) -> ResponseStream a -> IO ()
forEach f ResponseStream {_uri, _req} = requestMany _uri _req f
