{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Subscriptions.Apollo
  ( ApolloAction (..),
    apolloFormat,
    acceptApolloRequest,
    toApolloResponse,
    Validation,
    ApolloSubscription (..),
    ApolloMessageType (..),
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad.Fail (fail)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    eitherDecode,
    encode,
    pairs,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
  )
import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Data.Either
  ( Either (..),
    either,
  )
import Data.Functor ((<$>))
import Data.Maybe
  ( Maybe (..),
    maybe,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    Token,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    unpack,
  )
import GHC.Generics (Generic)
import Network.WebSockets
  ( AcceptRequest (..),
    Connection,
    PendingConnection,
    RequestHead,
    acceptRequestWith,
    getRequestSubprotocols,
    pendingRequest,
  )
import Prelude
  ( Eq,
    Show,
    String,
    mempty,
    return,
    ($),
    (.),
  )

type ID = Text

data ApolloSubscription payload = ApolloSubscription
  { apolloId :: Maybe ID,
    apolloType :: ApolloMessageType,
    apolloPayload :: Maybe payload
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (ApolloSubscription a) where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o =
        ApolloSubscription
          <$> o
            .:? "id"
            <*> o
            .: "type"
            <*> o
            .:? "payload"

data RequestPayload = RequestPayload
  { payloadOperationName :: Maybe FieldName,
    payloadQuery :: Maybe Token,
    payloadVariables :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON RequestPayload where
  parseJSON = withObject "ApolloPayload" objectParser
    where
      objectParser o =
        RequestPayload
          <$> o
            .:? "operationName"
            <*> o
            .:? "query"
            <*> o
            .:? "variables"

instance (ToJSON a) => ToJSON (ApolloSubscription a) where
  toEncoding (ApolloSubscription id' type' payload') =
    pairs $
      encodeMaybe "id" id'
        <> "type"
          .= type'
        <> encodeMaybe "payload" payload'
    where
      -- Messages should only include these fields when they have real values,
      -- for example the MessageAck response should only include the type and optionally
      -- extraneous data in the payload.
      -- Aeson < 2.0.0 has Keys as Text, >= 2.0.0 has Data.Aeson.Key.Key
      -- encodeMaybe :: ToJSON b => Text -> Maybe b -> Series
      encodeMaybe _ Nothing = Prelude.mempty
      encodeMaybe k (Just v) = k .= v

acceptApolloRequest ::
  (MonadIO m) =>
  PendingConnection ->
  m Connection
acceptApolloRequest pending =
  liftIO $
    acceptRequestWith
      pending
      (acceptApolloSubProtocol (pendingRequest pending))

acceptApolloSubProtocol :: RequestHead -> AcceptRequest
acceptApolloSubProtocol reqHead =
  apolloProtocol (getRequestSubprotocols reqHead)
  where
    apolloProtocol ["graphql-subscriptions"] =
      AcceptRequest (Just "graphql-subscriptions") []
    apolloProtocol ["graphql-ws"] =
      AcceptRequest (Just "graphql-ws") []
    apolloProtocol ["graphql-transport-ws"] =
      AcceptRequest (Just "graphql-transport-ws") []
    apolloProtocol _ = AcceptRequest Nothing []

toApolloResponse :: ApolloMessageType -> Maybe ID -> Maybe GQLResponse -> ByteString
toApolloResponse responseType sid_myb val_myb =
  encode $ ApolloSubscription sid_myb responseType val_myb

data ApolloMessageType
  = GqlConnectionAck
  | GqlConnectionError
  | GqlData
  | GqlError
  | GqlComplete
  | GqlConnectionInit
  | GqlSubscribe
  | GqlPing
  | GqlPong
  deriving (Eq, Show, Generic)

instance FromJSON ApolloMessageType where
  parseJSON = withText "ApolloMessageType" txtParser
    where
      txtParser "connection_ack" = return GqlConnectionAck
      txtParser "connection_error" = return GqlConnectionError
      txtParser "next" = return GqlData
      txtParser "error" = return GqlError
      txtParser "complete" = return GqlComplete
      txtParser "connection_init" = return GqlConnectionInit
      txtParser "subscribe" = return GqlSubscribe
      txtParser "ping" = return GqlPing
      txtParser "pong" = return GqlPong
      txtParser _ = fail "Invalid type encountered."

instance ToJSON ApolloMessageType where
  toEncoding = toEncoding . apolloResponseToProtocolMsgType

apolloResponseToProtocolMsgType :: ApolloMessageType -> Text
apolloResponseToProtocolMsgType GqlConnectionAck = "connection_ack"
apolloResponseToProtocolMsgType GqlConnectionError = "connection_error"
apolloResponseToProtocolMsgType GqlConnectionInit = "connection_init"
apolloResponseToProtocolMsgType GqlData = "next"
apolloResponseToProtocolMsgType GqlError = "error"
apolloResponseToProtocolMsgType GqlComplete = "complete"
apolloResponseToProtocolMsgType GqlSubscribe = "subscribe"
apolloResponseToProtocolMsgType GqlPing = "ping"
apolloResponseToProtocolMsgType GqlPong = "pong"

data ApolloAction
  = SessionStop ID
  | SessionStart ID GQLRequest
  | ConnectionInit
  | Ping

type Validation = Either ByteString

apolloFormat :: ByteString -> Validation ApolloAction
apolloFormat = validateReq . eitherDecode
  where
    validateReq :: Either String (ApolloSubscription RequestPayload) -> Validation ApolloAction
    validateReq = either (Left . pack) validateSub
    -------------------------------------
    validateSub :: ApolloSubscription RequestPayload -> Validation ApolloAction
    validateSub ApolloSubscription {apolloType = GqlConnectionInit} =
      pure ConnectionInit
    validateSub ApolloSubscription {apolloType = GqlPing} =
      pure Ping
    validateSub ApolloSubscription {apolloType = GqlSubscribe, apolloId, apolloPayload} =
      do
        sessionId <- validateSession apolloId
        payload <- validatePayload apolloPayload
        pure $ SessionStart sessionId payload
    validateSub ApolloSubscription {apolloType = GqlComplete, apolloId} =
      SessionStop <$> validateSession apolloId
    validateSub ApolloSubscription {apolloType} =
      Left $
        "Unknown Request type \""
          <> pack (unpack $ apolloResponseToProtocolMsgType apolloType)
          <> "\"."

    validateSession :: Maybe ID -> Validation ID
    validateSession = maybe (Left "\"id\" was not provided") Right
    -------------------------------------
    validatePayload = maybe (Left "\"payload\" was not provided") validatePayloadContent
    -------------------------------------
    validatePayloadContent
      RequestPayload
        { payloadQuery,
          payloadOperationName = operationName,
          payloadVariables = variables
        } = do
        query <- maybe (Left "\"payload.query\" was not provided") Right payloadQuery
        pure $ GQLRequest {query, operationName, variables}
