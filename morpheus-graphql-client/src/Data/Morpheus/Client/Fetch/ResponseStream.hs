{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.ResponseStream
  ( request,
    forEach,
    single,
    ResponseStream,
  )
where

import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client.Fetch (Args)
import Data.Morpheus.Client.Fetch.RequestType
  ( ClientTypeConstraint,
    Request (..),
    RequestType (__type),
    decodeResponse,
    isSubscription,
    toRequest,
  )
import Data.Morpheus.Client.Fetch.WebSockets (decodeMessage, useWS)
import Data.Morpheus.Client.Internal.Types
import Data.Morpheus.Subscriptions.Internal (ApolloSubscription (..))
import Data.Morpheus.Types.Internal.AST (OperationType (Subscription))
import qualified Data.Text as T
import Network.HTTP.Req
  ( POST (..),
    ReqBodyLbs (ReqBodyLbs),
    defaultHttpConfig,
    header,
    lbsResponse,
    req,
    responseBody,
    runReq,
    useURI,
  )
import qualified Network.HTTP.Req as R (Option)
import Network.WebSockets (receiveData, sendTextData)
import Relude hiding (ByteString)
import Text.URI

parseURI :: MonadFail m => String -> m URI
parseURI url = maybe (fail ("Invalid Endpoint: " <> show url <> "!")) pure (mkURI (T.pack url))

headers :: R.Option scheme
headers = header "Content-Type" "application/json"

post :: URI -> ByteString -> IO ByteString
post uri body = case useURI uri of
  Nothing -> fail ("Invalid Endpoint: " <> show uri <> "!")
  (Just (Left (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> headers))
  (Just (Right (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> headers))

requestSingle :: ClientTypeConstraint a => URI -> Request a -> IO (Either (FetchError a) a)
requestSingle uri r
  | isSubscription r = undefined
  | otherwise = decodeResponse <$> post uri (A.encode $ toRequest r)

requestMany :: ClientTypeConstraint a => URI -> Request a -> (ClientResult a -> IO ()) -> IO ()
requestMany uri r f
  | isSubscription r =
    undefined
  | otherwise = useWS uri app
  where
    app conn = do
      sendTextData conn (A.encode initMessage)
      -- send initial GQL Request for subscription
      sendTextData
        conn
        ( A.encode
            ApolloSubscription
              { apolloPayload = Just (toRequest r),
                apolloType = "start",
                apolloId = Just "01"
              }
        )
      -- handle GQL subscription responses
      forever $ do
        message <- receiveData conn
        f (decodeMessage message)

initMessage :: ApolloSubscription ()
initMessage = ApolloSubscription {apolloType = "connection_init", apolloPayload = Nothing, apolloId = Nothing}

-- endMessage :: Text -> ApolloSubscription ()
-- endMessage uid = ApolloSubscription {apolloType = "stop", apolloPayload = Nothing, apolloId = Just uid}

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
