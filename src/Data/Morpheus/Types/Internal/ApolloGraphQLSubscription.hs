{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Internal.ApolloGraphQLSubscription
  ( ApolloGraphQLSubscription
  , parseApolloGraphQLSubscription
  ) where

import           Data.Aeson                 (FromJSON (..), Value, decode, encode, withObject, (.:))
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Text                  (Text)
import           Data.Text.Lazy             (toStrict)
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           GHC.Generics               (Generic)

data ApolloGraphQLSubscription = ApolloGraphQLSubscription
  { id      :: Maybe Text
  , _type   :: Maybe Text
  , payload :: Value
  } deriving (Generic)

instance FromJSON ApolloGraphQLSubscription where
  parseJSON =
    withObject "ApolloGraphQLSubscription" $ \o ->
      ApolloGraphQLSubscription <$> o .: "id" <*> o .: "type" <*> o .: "payload"

parseApolloGraphQLSubscription :: ByteString -> Maybe Text
parseApolloGraphQLSubscription x = bsToText . encode . payload <$> decode x

bsToText :: ByteString -> Text
bsToText = toStrict . decodeUtf8
