{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server.Apollo
  ( ApolloQuery
  , apolloProtocol
  , toApolloResponse
  , parseApolloRequest
  , bsToText
  ) where

import           Data.Aeson                 (FromJSON (..), eitherDecode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Text                  (Text)
import           Data.Text.Lazy             (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8, encodeUtf8)
import           GHC.Generics               (Generic)
import           Network.WebSockets         (AcceptRequest (..))

data ApolloQuery = ApolloQuery
  { id      :: Maybe Int
  , payload :: Maybe Text
  , query   :: Text
  } deriving (Generic)

instance FromJSON ApolloQuery

apolloProtocol :: AcceptRequest
apolloProtocol = AcceptRequest (Just "graphql-subscriptions") []

toApolloResponse :: Text -> Text -- TODO: encode real dataType
toApolloResponse res = "{\"type\":\"subscription_data\",\"id\":0,\"payload\":" <> res <> "}"

parseApolloRequest :: Text -> Either String ApolloQuery
parseApolloRequest = eitherDecode . toLBS

toLBS :: Text -> ByteString
toLBS = encodeUtf8 . fromStrict

bsToText :: ByteString -> Text
bsToText = toStrict . decodeUtf8
