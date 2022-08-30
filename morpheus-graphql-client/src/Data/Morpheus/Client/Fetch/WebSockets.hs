{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.WebSockets
  ( useWS,
    decodeMessage,
  )
where

import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List.NonEmpty
import Data.Morpheus.Client.Fetch.RequestType (processResponse)
import Data.Morpheus.Client.Internal.Types (ClientResult, FetchError (..))
import Data.Morpheus.Client.Schema.JSON.Types (JSONResponse (..))
import Data.Morpheus.Subscriptions.Internal (ApolloSubscription (..))
import qualified Data.Text as T
import Network.WebSockets.Client (ClientApp, runClient)
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

useWS :: URI -> ClientApp () -> IO ()
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
