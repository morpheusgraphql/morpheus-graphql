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
    ApolloResponseType(..),
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    Series,
    eitherDecode,
    encode,
    pairs,
    withObject,
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
  ( Show,
    String,
    ($),
    (.),
    mempty,
  )

type ID = Text

data ApolloSubscription payload = ApolloSubscription
  { apolloId :: Maybe ID,
    apolloType :: Text,
    apolloPayload :: Maybe payload
  }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (ApolloSubscription a) where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o =
        ApolloSubscription
          <$> o .:? "id"
          <*> o .: "type"
          <*> o .:? "payload"

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
          <$> o .:? "operationName"
          <*> o .:? "query"
          <*> o .:? "variables"

instance ToJSON a => ToJSON (ApolloSubscription a) where
  toEncoding (ApolloSubscription id' type' payload') =
    pairs $ encodeMaybe "id" id'
          <> "type" .= type'
          <> encodeMaybe "payload" payload'
    where
      -- Messages should only include these fields when they have real values,
      -- for example the MessageAck response should only include the type and optionally
      -- extraneous data in the payload.
      encodeMaybe :: ToJSON b => Text -> Maybe b -> Series
      encodeMaybe k Nothing = Prelude.mempty
      encodeMaybe k (Just v) = k .= v

acceptApolloRequest ::
  MonadIO m =>
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

toApolloResponse :: ApolloResponseType -> Maybe ID -> Maybe GQLResponse -> ByteString
toApolloResponse responseType sid_myb val_myb =
  encode $ ApolloSubscription sid_myb (apolloResponseToProtocolMsgType responseType) val_myb

data ApolloResponseType
  = ConnectionAck
  | ConnectionError
  | GqlData
  | GqlError
  | GqlComplete

apolloResponseToProtocolMsgType :: ApolloResponseType -> Text
apolloResponseToProtocolMsgType ConnectionAck = "connection_ack"
apolloResponseToProtocolMsgType ConnectionError = "connection_error"
apolloResponseToProtocolMsgType GqlData = "next"
apolloResponseToProtocolMsgType GqlError = "error"
apolloResponseToProtocolMsgType GqlComplete = "complete"

data ApolloAction
  = SessionStop ID
  | SessionStart ID GQLRequest
  | ConnectionInit

type Validation = Either ByteString

apolloFormat :: ByteString -> Validation ApolloAction
apolloFormat = validateReq . eitherDecode
  where
    validateReq :: Either String (ApolloSubscription RequestPayload) -> Validation ApolloAction
    validateReq = either (Left . pack) validateSub
    -------------------------------------
    validateSub :: ApolloSubscription RequestPayload -> Validation ApolloAction
    validateSub ApolloSubscription {apolloType = "connection_init"} =
      pure ConnectionInit
    validateSub sub@ApolloSubscription{apolloType = "subscribe", apolloId, apolloPayload} =
      validateStartOrSubscribe sub
    validateSub sub@ApolloSubscription{apolloType = "start", apolloId, apolloPayload} =
      validateStartOrSubscribe sub
    validateSub ApolloSubscription {apolloType = "stop", apolloId} =
      SessionStop <$> validateSession apolloId
    validateSub ApolloSubscription {apolloType} =
      Left $ "Unknown Request type \"" <> pack (unpack apolloType) <> "\"."

    validateStartOrSubscribe :: ApolloSubscription RequestPayload -> Validation ApolloAction
    validateStartOrSubscribe ApolloSubscription {apolloType, apolloId, apolloPayload} =
      do
        sessionId <- validateSession apolloId
        payload <- validatePayload apolloPayload
        pure $ SessionStart sessionId payload
    --------------------------------------------
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
