{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server.Apollo
  ( SubAction(..)
  , apolloFormat
  , acceptApolloSubProtocol
  , toApolloResponse
  ) where

import           Data.Aeson                         (FromJSON (..),Value(..), ToJSON (..), eitherDecode, encode, pairs,
                                                     withObject, (.:), (.:?), (.=))
import           Data.ByteString.Lazy.Char8         (ByteString)
import           Data.Morpheus.Types                (GQLRequest (..))
import           Data.Morpheus.Types.IO             (GQLResponse)
import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           GHC.Generics                       (Generic)
import           Network.WebSockets                 (AcceptRequest (..), RequestHead, getRequestSubprotocols)

type ApolloID = Text

data ApolloSubscription payload = ApolloSubscription
  { apolloId      :: Maybe ApolloID
  , apolloType    :: Text
  , apolloPayload :: Maybe payload
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (ApolloSubscription a) where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o = ApolloSubscription <$> o .:? "id" <*> o .: "type" <*> o .:? "payload"

data RequestPayload = RequestPayload
  { payloadOperationName :: Maybe Text
  , payloadQuery         :: Maybe Text
  , payloadVariables     :: Maybe Value
  } deriving (Show, Generic)

instance FromJSON RequestPayload where
  parseJSON = withObject "ApolloPayload" objectParser
    where
      objectParser o = RequestPayload <$> o .:? "operationName" <*> o .:? "query" <*> o .:? "variables"

instance ToJSON a => ToJSON (ApolloSubscription a) where
  toEncoding (ApolloSubscription id' type' payload') = pairs $ "id" .= id' <> "type" .= type' <> "payload" .= payload'

acceptApolloSubProtocol :: RequestHead -> AcceptRequest
acceptApolloSubProtocol reqHead = apolloProtocol (getRequestSubprotocols reqHead)
  where
    apolloProtocol ["graphql-subscriptions"] = AcceptRequest (Just "graphql-subscriptions") []
    apolloProtocol ["graphql-ws"]            = AcceptRequest (Just "graphql-ws") []
    apolloProtocol _                         = AcceptRequest Nothing []

toApolloResponse :: ApolloID -> GQLResponse -> ByteString
toApolloResponse sid val = encode $ ApolloSubscription (Just sid) "data" (Just val)

data SubAction
  = RemoveSub ApolloID
  | AddSub ApolloID
           GQLRequest
  | SubError String

apolloFormat :: ByteString -> SubAction
apolloFormat = toWsAPI . eitherDecode
  where
    toWsAPI :: Either String (ApolloSubscription RequestPayload) -> SubAction
    toWsAPI (Right ApolloSubscription { apolloType = "start"
                                      , apolloId = Just sessionId
                                      , apolloPayload = Just RequestPayload { payloadQuery = Just query
                                                                                  , payloadOperationName = operationName
                                                                                  , payloadVariables = variables
                                                                                  }
                                      }) = AddSub sessionId (GQLRequest {query, operationName, variables})
    toWsAPI (Right ApolloSubscription {apolloType = "stop", apolloId = Just sessionId}) = RemoveSub sessionId
    toWsAPI (Right x) = SubError (show x)
    toWsAPI (Left x) = SubError x
