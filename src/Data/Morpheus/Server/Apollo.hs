{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server.Apollo
  ( ApolloSubscription(..)
  , apolloProtocol
  , toApolloResponse
  , parseApolloRequest
  , bsToText
  ) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value (..), decode, eitherDecode, encode,
                                             object, withObject, (.:), (.:?), (.=))
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Text                  (Text)
import           Data.Text.Lazy             (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8, encodeUtf8)
import           GHC.Generics               (Generic)
import           Network.WebSockets         (AcceptRequest (..))

data ApolloSubscription = ApolloSubscription
  { apolloId      :: Maybe Int
  , apolloType    :: Text
  , apolloPayload :: Maybe Value
  , apolloQuery   :: Maybe Text
  } deriving (Generic)

instance FromJSON ApolloSubscription where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o = ApolloSubscription <$> o .:? "id" <*> o .: "type" <*> o .:? "payload" <*> o .:? "query"

instance ToJSON ApolloSubscription where
  toJSON (ApolloSubscription id' type' payload' query') =
    object ["id" .= id', "type" .= type', "payload" .= payload', "query" .= query']

apolloProtocol :: AcceptRequest
apolloProtocol = AcceptRequest (Just "graphql-subscriptions") []

-- TODO:  Value -> Text
toApolloResponse :: Text -> Text
toApolloResponse val' = bsToText $ encode $ ApolloSubscription (Just 0) "subscription_data" toValue Nothing
  where
    toValue = (decode $ toLBS val') :: Maybe Value

parseApolloRequest :: Text -> Either String ApolloSubscription
parseApolloRequest = eitherDecode . toLBS

toLBS :: Text -> ByteString
toLBS = encodeUtf8 . fromStrict

bsToText :: ByteString -> Text
bsToText = toStrict . decodeUtf8
