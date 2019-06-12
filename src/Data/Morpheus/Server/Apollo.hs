{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server.Apollo
  ( ApolloSubscription(..)
  , apolloProtocol
  , toApolloResponse
  , parseApolloRequest
  ) where

import           Data.Aeson                         (FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode,
                                                     pairs, withObject, (.:), (.:?), (.=))
import           Data.ByteString.Lazy.Char8         (ByteString)
import           Data.Map                           (Map)
import qualified Data.Morpheus.Types.Internal.Value as V (Value)
import           Data.Morpheus.Types.Response       (GQLResponse)
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Network.WebSockets                 (AcceptRequest (..))

data ApolloSubscription a = ApolloSubscription
  { apolloId            :: Maybe Int
  , apolloType          :: Text
  , apolloPayload       :: Maybe a
  , apolloQuery         :: Maybe Text
  , apolloOperationName :: Maybe Text
  , apolloVariables     :: Maybe (Map Text V.Value)
  } deriving (Generic)

instance FromJSON (ApolloSubscription Value) where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o =
        ApolloSubscription <$> o .:? "id" <*> o .: "type" <*> o .:? "payload" <*> o .:? "query" <*>
        o .:? "operationName" <*>
        o .:? "variables"

instance ToJSON (ApolloSubscription GQLResponse) where
  toEncoding (ApolloSubscription id' type' payload' query' operationName' variables') =
    pairs $
    "id" .= id' <> "type" .= type' <> "payload" .= payload' <> "query" .= query' <> "operationName" .= operationName' <>
    "variables" .=
    variables'

apolloProtocol :: AcceptRequest
apolloProtocol = AcceptRequest (Just "graphql-subscriptions") []

toApolloResponse :: Int -> GQLResponse -> ByteString
toApolloResponse sid' val' =
  encode $ ApolloSubscription (Just sid') "subscription_data" (Just val') Nothing Nothing Nothing

parseApolloRequest :: ByteString -> Either String (ApolloSubscription Value)
parseApolloRequest = eitherDecode
