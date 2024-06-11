{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.WebSockets
  ( useWS,
    sendInitialRequest,
    responseStream,
    sendRequest,
    receiveResponse,
    endSession,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map as M
import Data.Morpheus.Client.Fetch.GQLClient (Headers)
import Data.Morpheus.Client.Fetch.RequestType (Request, RequestType (..), processResponse, toRequest)
import Data.Morpheus.Client.Fetch.Types (FetchError (..), GQLClientResult)
import Data.Morpheus.Client.Schema.JSON.Types (JSONResponse (..))
import Data.Morpheus.Subscriptions (ApolloMessageType (..))
import Data.Morpheus.Subscriptions.Internal (ApolloSubscription (..))
import qualified Data.Text as T
import Network.WebSockets.Client (runClientWith)
import Network.WebSockets.Connection (Connection, defaultConnectionOptions, receiveData, sendTextData)
import Relude hiding (ByteString)
import Text.URI
  ( Authority (..),
    RText,
    RTextLabel (..),
    URI (..),
    unRText,
  )
import Wuss (runSecureClientWith)

handleHost :: Text -> String
handleHost "localhost" = "127.0.0.1"
handleHost x = T.unpack x

toPort :: Maybe Word -> Int
toPort Nothing = 80
toPort (Just x) = fromIntegral x

getPath :: Maybe (Bool, NonEmpty (RText 'PathPiece)) -> String
getPath (Just (_, h :| t)) = T.unpack $ T.intercalate "/" $ fmap unRText (h : t)
getPath _ = ""

data WebSocketSettings = WebSocketSettings
  { isSecure :: Bool,
    port :: Int,
    host :: String,
    path :: String,
    headers :: Headers
  }
  deriving (Show)

parseProtocol :: (MonadFail m) => Text -> m Bool
parseProtocol "ws" = pure False
parseProtocol "wss" = pure True
parseProtocol p = fail $ "unsupported protocol" <> show p

getWebsocketURI :: (MonadFail m) => URI -> Headers -> m WebSocketSettings
getWebsocketURI URI {uriScheme = Just scheme, uriAuthority = Right Authority {authHost, authPort}, uriPath} headers = do
  isSecure <- parseProtocol $ unRText scheme
  pure
    WebSocketSettings
      { isSecure,
        host = handleHost $ unRText authHost,
        port = toPort authPort,
        path = getPath uriPath,
        headers
      }
getWebsocketURI uri _ = fail ("Invalid Endpoint: " <> show uri <> "!")

toHeader :: (IsString a) => (Text, Text) -> (a, BS.ByteString)
toHeader (x, y) = (fromString $ T.unpack x, BS.pack $ T.unpack y)

_useWS :: WebSocketSettings -> (Connection -> IO a) -> IO a
_useWS WebSocketSettings {isSecure, ..} app
  | isSecure = runSecureClientWith host 443 path options wsHeaders app
  | otherwise = runClientWith host port path options wsHeaders app
  where
    wsHeaders = map toHeader $ M.toList headers
    options = defaultConnectionOptions

useWS :: (MonadFail m, MonadIO m, MonadUnliftIO m) => URI -> Headers -> (Connection -> m a) -> m a
useWS uri headers app = do
  wsURI <- getWebsocketURI uri headers
  withRunInIO $ \runInIO -> _useWS wsURI (runInIO . app)

processMessage :: ApolloSubscription (JSONResponse a) -> GQLClientResult a
processMessage ApolloSubscription {apolloPayload = Just payload} = processResponse payload
processMessage ApolloSubscription {} = Left (FetchErrorParseFailure "empty message")

decodeMessage :: (A.FromJSON a) => ByteString -> GQLClientResult a
decodeMessage = (first FetchErrorParseFailure . A.eitherDecode) >=> processMessage

initialMessage :: ApolloSubscription ()
initialMessage = ApolloSubscription {apolloType = GqlConnectionInit, apolloPayload = Nothing, apolloId = Nothing}

encodeRequestMessage :: (RequestType a, A.ToJSON (RequestArgs a)) => Text -> Request a -> ByteString
encodeRequestMessage uid r =
  A.encode
    ApolloSubscription
      { apolloPayload = Just (toRequest r),
        apolloType = GqlSubscribe,
        apolloId = Just uid
      }

endMessage :: Text -> ApolloSubscription ()
endMessage uid = ApolloSubscription {apolloType = GqlComplete, apolloPayload = Nothing, apolloId = Just uid}

endSession :: (MonadIO m) => Connection -> Text -> m ()
endSession conn uid = liftIO $ sendTextData conn $ A.encode $ endMessage uid

receiveResponse :: (MonadIO m) => (A.FromJSON a) => Connection -> m (GQLClientResult a)
receiveResponse conn = liftIO $ do
  message <- receiveData conn
  pure $ decodeMessage message

-- returns infinite number of responses
responseStream :: (A.FromJSON a, MonadIO m) => Connection -> [m (GQLClientResult a)]
responseStream conn = getResponse : responseStream conn
  where
    getResponse = receiveResponse conn

sendRequest :: (RequestType a, A.ToJSON (RequestArgs a), MonadIO m) => Connection -> Text -> Request a -> m ()
sendRequest conn uid r = liftIO $ sendTextData conn (encodeRequestMessage uid r)

sendInitialRequest :: (MonadIO m) => Connection -> m ()
sendInitialRequest conn = liftIO $ sendTextData conn (A.encode initialMessage)
