{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.IO
  ( GQLRequest(..)
  , GQLResponse(..)
  , JSONResponse(..)
  , renderResponse
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , pairs
                                                , withObject
                                                , (.:?)
                                                , (.=)
                                                )
import qualified Data.Aeson                    as Aeson
                                                ( Value(..) )
import qualified Data.HashMap.Lazy             as LH
                                                ( toList )
import           GHC.Generics                   ( Generic )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key )
import           Data.Morpheus.Types.Internal.Validation
                                                ( JSONError(..)
                                                , Validation(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )
import           Data.Morpheus.Error.Utils      ( renderErrors )

renderResponse :: Validation Value -> GQLResponse
renderResponse Failure { validationErrors } =
  Errors $ renderErrors validationErrors
renderResponse Success { validationValue } = Data validationValue

instance FromJSON a => FromJSON (JSONResponse a) where
  parseJSON = withObject "JSONResponse" objectParser
    where objectParser o = JSONResponse <$> o .:? "data" <*> o .:? "errors"

data JSONResponse a = JSONResponse
  { responseData   :: Maybe a
  , responseErrors :: Maybe [JSONError]
  } deriving (Generic, Show, ToJSON)

-- | GraphQL HTTP Request Body
data GQLRequest = GQLRequest
  { query         :: Key
  , operationName :: Maybe Key
  , variables     :: Maybe Aeson.Value
  } deriving (Show, Generic, FromJSON, ToJSON)

-- | GraphQL Response
data GQLResponse
  = Data Value
  | Errors [JSONError]
  deriving (Show, Generic)

instance FromJSON GQLResponse where
  parseJSON (Aeson.Object hm) = case LH.toList hm of
    [("data"  , value)] -> Data <$> parseJSON value
    [("errors", value)] -> Errors <$> parseJSON value
    _                   -> fail "Invalid GraphQL Response"
  parseJSON _ = fail "Invalid GraphQL Response"

instance ToJSON GQLResponse where
  toEncoding (Data   _data  ) = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors
