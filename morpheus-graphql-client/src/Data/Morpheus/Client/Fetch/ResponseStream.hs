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
    morpheusClient,
    MorpheusClient (clientHeaders),
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
  ( endSession,
    receiveResponse,
    responseStream,
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
  | isSubscription r = useWS uri wsApp
  | otherwise = decodeResponse <$> post uri (A.encode $ toRequest r)
  where
    wsApp conn = do
      let sid = "0243134"
      sendInitialRequest conn
      sendRequest conn sid r
      x <- receiveResponse conn
      endSession conn sid
      pure x

requestMany :: ClientTypeConstraint a => URI -> Request a -> (ClientResult a -> IO ()) -> IO ()
requestMany uri r f
  | isSubscription r = useWS uri appWS
  | otherwise = post uri (A.encode $ toRequest r) >>= f . decodeResponse
  where
    appWS conn = do
      let sid = "0243134"
      sendInitialRequest conn
      sendRequest conn sid r
      traverse_ (>>= f) (responseStream conn)
      endSession conn sid

type Headers = Map String String

data MorpheusClient = MorpheusClient
  { clientHeaders :: Headers,
    clientURI :: String
  }

morpheusClient :: String -> MorpheusClient
morpheusClient clientURI =
  MorpheusClient
    { clientURI,
      clientHeaders = fromList [("Content-Type", "application/json")]
    }

-- PUBLIC API
data ResponseStream a = ClientTypeConstraint a =>
  ResponseStream
  { _req :: Request a,
    _uri :: URI,
    _headers :: Headers
    -- _wsConnection :: Connection
  }

request :: ClientTypeConstraint a => MorpheusClient -> Args a -> IO (ResponseStream a)
request MorpheusClient {clientURI, clientHeaders} requestArgs = do
  _uri <- parseURI clientURI
  let _req = Request {requestArgs}
  pure ResponseStream {_req, _uri, _headers = clientHeaders}

-- | returns first response from the server
single :: ResponseStream a -> IO (ClientResult a)
single ResponseStream {_req, _uri} = requestSingle _uri _req

-- | returns loop listening subscription events forever. if you want to run it in background use `forkIO`
forEach :: (ClientResult a -> IO ()) -> ResponseStream a -> IO ()
forEach f ResponseStream {_uri, _req} = requestMany _uri _req f
