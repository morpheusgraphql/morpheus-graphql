{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server.Apollo
  ( ApolloAction(..)
  , apolloFormat
  , acceptApolloSubProtocol
  , toApolloResponse
  ) where

import           Data.Aeson                         (FromJSON (..), ToJSON (..), eitherDecode, encode, pairs,
                                                     withObject, (.:), (.:?), (.=))
import           Data.ByteString.Lazy.Char8         (ByteString)
import           Data.Map                           (Map)
import           Data.Morpheus.Types                (GQLRequest (..))
import qualified Data.Morpheus.Types.Internal.Value as V (Value)
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
  } deriving (Generic)

instance FromJSON a => FromJSON (ApolloSubscription a) where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o = ApolloSubscription <$> o .:? "id" <*> o .: "type" <*> o .:? "payload"

data ApolloRequestPayload = ApolloRequestPayload
  { payloadOperationName :: Maybe Text
  , payloadQuery         :: Maybe Text
  , payloadVariables     :: Maybe (Map Text V.Value)
  } deriving (Generic)

instance FromJSON ApolloRequestPayload where
  parseJSON = withObject "ApolloPayload" objectParser
    where
      objectParser o = ApolloRequestPayload <$> o .:? "operationName" <*> o .:? "query" <*> o .:? "variables"

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

data ApolloAction
  = ApolloRemove ApolloID
  | ApolloError String
  | ApolloRequest ApolloID
                  GQLRequest
  | ApolloNoAction

apolloFormat :: ByteString -> ApolloAction
apolloFormat = toWsAPI . eitherDecode
  where
    toWsAPI :: Either String (ApolloSubscription ApolloRequestPayload) -> ApolloAction
    toWsAPI (Left x) = ApolloError x
    toWsAPI (Right ApolloSubscription {apolloType = "stop", apolloId = Just sessionId}) = ApolloRemove sessionId
    toWsAPI (Right ApolloSubscription { apolloType = "start"
                                      , apolloId = Just sessionId
                                      , apolloPayload = Just ApolloRequestPayload { payloadQuery = Just query
                                                                                  , payloadOperationName = operationName
                                                                                  , payloadVariables = variables
                                                                                  }
                                      }) = ApolloRequest sessionId (GQLRequest {query, operationName, variables})
    toWsAPI (Right _) = ApolloNoAction
