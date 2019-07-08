{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server.Apollo
  ( ApolloAction(..)
  , oldApolloFormat
  , acceptApolloSubProtocol
  , toApolloResponse
  ) where

import           Data.Aeson                         (FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode,
                                                     pairs, withObject, (.:), (.:?), (.=))
import           Data.ByteString.Lazy.Char8         (ByteString)
import           Data.Map                           (Map)
import           Data.Morpheus.Types                (GQLRequest (..))
import qualified Data.Morpheus.Types.Internal.Value as V (Value)
import           Data.Morpheus.Types.IO             (GQLResponse)
import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Network.WebSockets                 (AcceptRequest (..), RequestHead, getRequestSubprotocols)

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

acceptApolloSubProtocol :: RequestHead -> AcceptRequest
acceptApolloSubProtocol reqHead = apolloProtocol (getRequestSubprotocols reqHead)
  where
    apolloProtocol ["graphql-subscriptions"] = AcceptRequest (Just "graphql-subscriptions") []
    apolloProtocol ["graphql-ws"]            = AcceptRequest (Just "graphql-ws") []
    apolloProtocol _                         = AcceptRequest Nothing []

toApolloResponse :: Int -> GQLResponse -> ByteString
toApolloResponse sid' val' =
  encode $ ApolloSubscription (Just sid') "subscription_data" (Just val') Nothing Nothing Nothing

parseApolloRequest :: ByteString -> Either String (ApolloSubscription Value)
parseApolloRequest = eitherDecode

data ApolloAction
  = ApolloRemove Int
  | ApolloError String
  | ApolloRequest Int
                  GQLRequest
  | ApolloNoAction

oldApolloFormat :: ByteString -> ApolloAction
oldApolloFormat = toWsAPI . parseApolloRequest
  where
    toWsAPI (Left x) = ApolloError x
    toWsAPI (Right ApolloSubscription {apolloType = "subscription_end", apolloId = Just sid}) = ApolloRemove sid
    toWsAPI (Right ApolloSubscription { apolloType = "subscription_start"
                                      , apolloId = Just sessionId
                                      , apolloQuery = Just query
                                      , apolloOperationName = operationName
                                      , apolloVariables = variables
                                      }) = ApolloRequest sessionId (GQLRequest {query, operationName, variables})
    toWsAPI (Right _) = ApolloNoAction
