{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.IO
  ( GQLRequest(..)
  , GQLResponse(..)
  , JSONResponse(..)
  , renderResponse
  , Response(..)
  )
where

import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , pairs
                                                , withObject
                                                , (.:?)
                                                , (.=)
                                                , object
                                                )
import qualified Data.Aeson                    as Aeson
                                                ( Value(..) )
import qualified Data.HashMap.Lazy             as LH
                                                ( toList )
import           GHC.Generics                   ( Generic )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( GQLError(..)
                                                , Result(..)
                                                , ResultT(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )

data Response actions = Response {
  resErrors :: Maybe [GQLError],
  resData :: Maybe Value,
  resActions :: [actions]
} deriving (Generic)

instance ToJSON (Response actons) where
  toEncoding Response { resData = Just x }    = pairs $ "data" .= x
  toEncoding Response { resErrors = Just er } = pairs $ "errors" .= er
  toEncoding Response{}                       = toEncoding ("Panic" :: String)
  toJSON Response { resData, resErrors } =
    object ["data" .= resData, "errors" .= resErrors]

renderResponse :: Result e con GQLError Value -> GQLResponse
renderResponse (Failure errors)   = Errors errors
renderResponse Success { result } = Data result

instance FromJSON a => FromJSON (JSONResponse a) where
  parseJSON = withObject "JSONResponse" objectParser
    where objectParser o = JSONResponse <$> o .:? "data" <*> o .:? "errors"

data JSONResponse a = JSONResponse
  { responseData   :: Maybe a
  , responseErrors :: Maybe [GQLError]
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
  | Errors [GQLError]
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
