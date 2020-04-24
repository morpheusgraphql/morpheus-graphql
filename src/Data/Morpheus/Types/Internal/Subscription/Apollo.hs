{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Internal.Subscription.Apollo
  ( ApolloAction(..)
  , apolloFormat
  , acceptApolloRequest
  , toApolloResponse
  , Validation
  ) where

import           Data.Maybe                 ( maybe )        
import           Control.Monad.IO.Class     ( MonadIO(..) )
import           Data.Aeson                 ( FromJSON (..)
                                            , ToJSON (..)
                                            , Value (..)
                                            , eitherDecode
                                            , encode
                                            , pairs
                                            , withObject
                                            , (.:)
                                            , (.:?)
                                            , (.=)
                                            )
import           Data.ByteString.Lazy.Char8 ( ByteString
                                            , pack
                                            )
import           Data.Semigroup             ( (<>) )
import           Data.Text                  ( Text 
                                            , unpack
                                            )
import           GHC.Generics               ( Generic )
import           Network.WebSockets         ( AcceptRequest (..)
                                            , RequestHead
                                            , getRequestSubprotocols
                                            , PendingConnection
                                            , Connection
                                            , acceptRequestWith
                                            , pendingRequest
                                            )

-- MORPHEUS
import           Data.Morpheus.Types.IO     ( GQLResponse 
                                            , GQLRequest (..)
                                            )

type ID = Text

data ApolloSubscription payload =
  ApolloSubscription
    { apolloId      :: Maybe ID
    , apolloType    :: Text
    , apolloPayload :: Maybe payload
    }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (ApolloSubscription a) where
  parseJSON = withObject "ApolloSubscription" objectParser
    where
      objectParser o =
        ApolloSubscription <$> o .:? "id" <*> o .: "type" <*> o .:? "payload"

data RequestPayload =
  RequestPayload
    { payloadOperationName :: Maybe Text
    , payloadQuery         :: Maybe Text
    , payloadVariables     :: Maybe Value
    }
  deriving (Show, Generic)

instance FromJSON RequestPayload where
  parseJSON = withObject "ApolloPayload" objectParser
    where
      objectParser o =
        RequestPayload <$> o .:? "operationName" <*> o .:? "query" <*>
        o .:? "variables"

instance ToJSON a => ToJSON (ApolloSubscription a) where
  toEncoding (ApolloSubscription id' type' payload') =
    pairs $ "id" .= id' <> "type" .= type' <> "payload" .= payload'

acceptApolloRequest 
  :: MonadIO m 
  => PendingConnection 
  -> m Connection
acceptApolloRequest pending 
  = liftIO 
    $ acceptRequestWith
        pending
        (acceptApolloSubProtocol (pendingRequest pending))

acceptApolloSubProtocol :: RequestHead -> AcceptRequest
acceptApolloSubProtocol reqHead =
  apolloProtocol (getRequestSubprotocols reqHead)
  where
    apolloProtocol ["graphql-subscriptions"] =
      AcceptRequest (Just "graphql-subscriptions") []
    apolloProtocol ["graphql-ws"] = AcceptRequest (Just "graphql-ws") []
    apolloProtocol _ = AcceptRequest Nothing []

toApolloResponse :: ID -> GQLResponse -> ByteString
toApolloResponse sid val =
  encode $ ApolloSubscription (Just sid) "data" (Just val)

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
    validateSub :: ApolloSubscription RequestPayload ->  Validation ApolloAction
    validateSub ApolloSubscription { apolloType = "connection_init" }
      = pure ConnectionInit 
    validateSub ApolloSubscription { apolloType = "start", apolloId , apolloPayload }
      = do
        sessionId <- validateSession apolloId
        payload   <- validatePayload apolloPayload
        pure $ SessionStart sessionId payload
    validateSub ApolloSubscription { apolloType = "stop", apolloId }
      = SessionStop <$> validateSession apolloId
    validateSub ApolloSubscription { apolloType } 
      = Left $ "Unknown Request type \""<> pack (unpack apolloType) <> "\"."
    --------------------------------------------
    validateSession :: Maybe ID -> Validation ID
    validateSession = maybe (Left "\"id\" was not provided") Right
    -------------------------------------
    validatePayload = maybe (Left "\"payload\" was not provided") validatePayloadContent
    -------------------------------------
    validatePayloadContent RequestPayload 
          { payloadQuery 
          , payloadOperationName = operationName
          , payloadVariables = variables
          } = do
            query <- maybe (Left "\"payload.query\" was not provided") Right payloadQuery
            pure $ GQLRequest {query, operationName, variables}