{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.IO
  ( GQLRequest (..),
    GQLResponse (..),
    JSONResponse (..),
    renderResponse,
  )
where

import Data.Aeson
  ( (.:?),
    (.=),
    FromJSON (..),
    ToJSON (..),
    object,
    pairs,
    withObject,
  )
import qualified Data.Aeson as Aeson
  ( Value (..),
  )
import qualified Data.HashMap.Lazy as LH
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError (..),
    ValidValue,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Result (..),
  )
import Relude hiding
  ( decodeUtf8,
    encodeUtf8,
  )

renderResponse :: Result e ValidValue -> GQLResponse
renderResponse (Failure errors) = Errors (sortOn locations errors)
renderResponse Success {result} = Data result

instance FromJSON a => FromJSON (JSONResponse a) where
  parseJSON = withObject "JSONResponse" objectParser
    where
      objectParser o = JSONResponse <$> o .:? "data" <*> o .:? "errors"

data JSONResponse a = JSONResponse
  { responseData :: Maybe a,
    responseErrors :: Maybe [GQLError]
  }
  deriving (Generic, Show, ToJSON)

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
  parseJSON (Aeson.Object hm) = case LH.toList hm of
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
