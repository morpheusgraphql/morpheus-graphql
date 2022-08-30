{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List.NonEmpty
import Data.Morpheus.Client.Fetch.RequestType (Request, RequestType (..), processResponse, toRequest)
import Data.Morpheus.Client.Internal.Types (ClientResult, FetchError (..))
import Data.Morpheus.Client.Schema.JSON.Types (JSONResponse (..))
import Data.Morpheus.Subscriptions.Internal (ApolloSubscription (..))
import qualified Data.Text as T
import Network.WebSockets.Client (runClient)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData)
import Relude hiding (ByteString)
import Text.URI

handleHost :: Text -> String
handleHost "localhost" = "127.0.0.1"
handleHost x = T.unpack x

toPort :: Maybe Word -> Int
toPort Nothing = 80
toPort (Just x) = fromIntegral x

getPath :: Maybe (Bool, NonEmpty (RText 'PathPiece)) -> String
getPath (Just (_, h :| t)) = T.unpack $ T.intercalate "/" $ fmap unRText (h : t)
getPath _ = ""

useWS :: URI -> (Connection -> IO a) -> IO a
useWS URI {uriScheme = Just scheme, uriAuthority = Right Authority {authHost, authPort}, uriPath} app
  | unRText scheme == "ws" = do
    let rPath = getPath uriPath
    let rPort = toPort authPort
    let rHost = handleHost $ unRText authHost
    runClient rHost rPort rPath app
useWS uri _ = fail ("Invalid Endpoint: " <> show uri <> "!")

processMessage :: ApolloSubscription (JSONResponse a) -> ClientResult a
processMessage ApolloSubscription {apolloPayload = Just payload} = processResponse payload
processMessage ApolloSubscription {} = Left (FetchErrorParseFailure "empty message")

decodeMessage :: A.FromJSON a => ByteString -> ClientResult a
decodeMessage = (first FetchErrorParseFailure . A.eitherDecode) >=> processMessage

initialMessage :: ApolloSubscription ()
initialMessage = ApolloSubscription {apolloType = "connection_init", apolloPayload = Nothing, apolloId = Nothing}

encodeRequestMessage :: (RequestType a, A.ToJSON (RequestArgs a)) => Text -> Request a -> ByteString
encodeRequestMessage uid r =
  A.encode
    ApolloSubscription
      { apolloPayload = Just (toRequest r),
        apolloType = "start",
        apolloId = Just uid
      }

endMessage :: Text -> ApolloSubscription ()
endMessage uid = ApolloSubscription {apolloType = "stop", apolloPayload = Nothing, apolloId = Just uid}

endSession :: Connection -> Text -> IO ()
endSession conn uid = sendTextData conn $ A.encode $ endMessage uid

receiveResponse :: A.FromJSON a => Connection -> IO (ClientResult a)
receiveResponse conn = do
  message <- receiveData conn
  pure $ decodeMessage message

-- returns infinite number of responses
responseStream :: (A.FromJSON a) => Connection -> [IO (ClientResult a)]
responseStream conn = getResponse : responseStream conn
  where
    getResponse = receiveResponse conn

sendRequest :: (RequestType a, A.ToJSON (RequestArgs a)) => Connection -> Text -> Request a -> IO ()
sendRequest conn uid r = sendTextData conn (encodeRequestMessage uid r)

sendInitialRequest :: Connection -> IO ()
sendInitialRequest conn = sendTextData conn (A.encode initialMessage)