{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server.Apollo
  ( ApolloAction(..)
  , oldApolloFormat
  , newApolloFormat
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

data ApolloSubscription sID payload = ApolloSubscription
  { apolloId            :: Maybe sID
  , apolloType          :: Text
  , apolloPayload       :: Maybe payload
  , apolloQuery         :: Maybe Text
  , apolloOperationName :: Maybe Text
  , apolloVariables     :: Maybe (Map Text V.Value)
  } deriving (Generic)

instance (FromJSON sID, FromJSON a) => FromJSON (ApolloSubscription sID a) where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o =
        ApolloSubscription <$> o .:? "id" <*> o .: "type" <*> o .:? "payload" <*> o .:? "query" <*>
        o .:? "operationName" <*>
        o .:? "variables"

data ApolloPayload = ApolloPayload
  { payloadOperationName :: Maybe Text
  , payloadQuery         :: Maybe Text
  , payloadVariables     :: Maybe (Map Text V.Value)
  } deriving (Generic)

instance FromJSON ApolloPayload where
  parseJSON = withObject "ApolloPayload" objectParser
    where
      objectParser o = ApolloPayload <$> o .:? "operationName" <*> o .:? "query" <*> o .:? "variables"

instance (ToJSON sID, ToJSON a) => ToJSON (ApolloSubscription sID a) where
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

toApolloResponse :: (Eq sID, ToJSON sID) => sID -> GQLResponse -> ByteString
toApolloResponse sid val = encode $ ApolloSubscription (Just sid) "data" (Just val) Nothing Nothing Nothing

data ApolloAction sID
  = ApolloRemove sID
  | ApolloError String
  | ApolloRequest sID
                  GQLRequest
  | ApolloNoAction

newApolloFormat :: ByteString -> ApolloAction Text
newApolloFormat = toWsAPI . eitherDecode
  where
    toWsAPI :: Either String (ApolloSubscription Text ApolloPayload) -> ApolloAction Text
    toWsAPI (Left x) = ApolloError x
    toWsAPI (Right ApolloSubscription {apolloType = "end", apolloId = Just sid}) = ApolloRemove sid
    toWsAPI (Right ApolloSubscription { apolloType = "start"
                                      , apolloId = Just sessionId
                                      , apolloPayload = Just ApolloPayload { payloadQuery = Just query
                                                                           , payloadOperationName = operationName
                                                                           , payloadVariables = variables
                                                                           }
                                      }) = ApolloRequest sessionId (GQLRequest {query, operationName, variables})
    toWsAPI (Right _) = ApolloNoAction

oldApolloFormat :: ByteString -> ApolloAction Int
oldApolloFormat = toWsAPI . eitherDecode
  where
    toWsAPI :: Either String (ApolloSubscription Int Value) -> ApolloAction Int
    toWsAPI (Left x) = ApolloError x
    toWsAPI (Right ApolloSubscription {apolloType = "subscription_end", apolloId = Just sid}) = ApolloRemove sid
    toWsAPI (Right ApolloSubscription { apolloType = "subscription_start"
                                      , apolloId = Just sessionId
                                      , apolloQuery = Just query
                                      , apolloOperationName = operationName
                                      , apolloVariables = variables
                                      }) = ApolloRequest sessionId (GQLRequest {query, operationName, variables})
    toWsAPI (Right _) = ApolloNoAction
