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
import Data.List.NonEmpty
import Data.Morpheus.Client.Fetch (Args)
import Data.Morpheus.Client.Fetch.RequestType
  ( ClientTypeConstraint,
    Request (..),
    decodeResponse,
    processResponse,
    toRequest,
  )
import Data.Morpheus.Client.Internal.Types
import Data.Morpheus.Client.Schema.JSON.Types (JSONResponse (..))
import Data.Morpheus.Subscriptions.Internal (ApolloSubscription (..))
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
import Network.WebSockets.Client (ClientApp, runClient)
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

toPort :: Maybe Word -> Int
toPort Nothing = 80
toPort (Just x) = fromIntegral x

getPath :: Maybe (Bool, NonEmpty (RText 'PathPiece)) -> String
getPath (Just (_, h :| t)) = T.unpack $ T.intercalate "/" $ fmap unRText (h : t)
getPath _ = ""

useWS :: URI -> ClientApp () -> IO ()
useWS URI {uriScheme = Just scheme, uriAuthority = Right Authority {authHost, authPort}, uriPath} app
  | unRText scheme == "ws" = do
    let rPath = getPath uriPath
    let rPort = toPort authPort
    let rHost = handleHost $ unRText authHost
    runClient rHost rPort rPath app
useWS uri _ = fail ("Invalid Endpoint: " <> show uri <> "!")

handleHost :: Text -> String
handleHost "localhost" = "127.0.0.1"
handleHost x = T.unpack x

requestSingle :: ClientTypeConstraint a => URI -> Request a -> IO (Either (FetchError a) a)
requestSingle uri r = decodeResponse <$> post uri (A.encode $ toRequest r)

requestMany :: ClientTypeConstraint a => URI -> Request a -> (ClientResult a -> IO ()) -> IO ()
requestMany uri r f = useWS uri app
  where
    decodeMessage = (first FetchErrorParseFailure . A.eitherDecode) >=> processMessage
    processMessage :: ApolloSubscription (JSONResponse a) -> Either (FetchError a) a
    processMessage ApolloSubscription {apolloPayload = Just payload} = processResponse payload
    processMessage ApolloSubscription {} = Left (FetchErrorParseFailure "empty message")
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
