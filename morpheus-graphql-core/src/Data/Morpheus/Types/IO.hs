{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
    renderResponse,
  )
where

import Data.Aeson
  ( (.=),
    FromJSON (..),
    ToJSON (..),
    object,
    pairs,
  )
import qualified Data.Aeson as Aeson
  ( Value (..),
  )
import qualified Data.Aeson.KeyMap as KM
import Data.Morpheus.Ext.Result
  ( Result (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError (..),
    GQLError,
    ValidValue,
  )
import Relude hiding
  ( decodeUtf8,
    encodeUtf8,
  )

renderResponse :: Result GQLError ValidValue -> GQLResponse
renderResponse (Failure errors) = Errors $ sort $ toList errors
renderResponse Success {result} = Data result

-- | GraphQL HTTP Request Body
data GQLRequest = GQLRequest
  { operationName :: Maybe FieldName,
    query :: Text,
    variables :: Maybe Aeson.Value
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | GraphQL Response
data GQLResponse
  = Data ValidValue
  | Errors [GQLError]
  deriving (Show, Generic)

instance FromJSON GQLResponse where
  parseJSON (Aeson.Object hm) = case KM.toList hm of
    [("data", value)] -> Data <$> parseJSON value
    [("errors", value)] -> Errors <$> parseJSON value
    _ -> fail "Invalid GraphQL Response"
  parseJSON _ = fail "Invalid GraphQL Response"

instance ToJSON GQLResponse where
  toJSON (Data gqlData) = object ["data" .= toJSON gqlData]
  toJSON (Errors errors) = object ["errors" .= toJSON errors]

  ----------------------------------------------------------
  toEncoding (Data _data) = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors
