{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.Morpheus.Types.IO
  ( GQLRequest(..)
  , GQLResponse(..)
  , JSONResponse(..)
  , renderResponse
  , MapAPI(..)
  )
where


import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , pairs
                                                , withObject
                                                , (.:?)
                                                , (.=)
                                                , object
                                                , encode
                                                )
import           Data.Aeson.Parser              ( eitherDecodeWith
                                                , jsonNoDup
                                                )
import qualified Data.Aeson                    as Aeson
                                                ( Value(..) )
import qualified Data.HashMap.Lazy             as LH
                                                ( toList )
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as LT
                                                ( Text
                                                , fromStrict
                                                , toStrict
                                                )
import           Data.Text.Lazy.Encoding        ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as LB
                                                ( ByteString
                                                , fromStrict
                                                , toStrict
                                                )
import           Data.Aeson.Internal            ( formatError
                                                , ifromJSON
                                                )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( badRequestError )
import           Data.Morpheus.Types.Internal.AST
                                                ( Key 
                                                , GQLError(..)
                                                , ValidValue
                                                )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Result(..)
                                                , Failure(..)
                                                )


decodeNoDup :: Failure String m => LB.ByteString -> m GQLRequest
decodeNoDup str = case eitherDecodeWith jsonNoDup ifromJSON str of
  Left  (path, x) -> failure $ formatError path x
  Right value     -> pure value

class MapAPI a where 
  mapAPI :: Applicative m => (GQLRequest -> m GQLResponse) -> a -> m a

instance MapAPI LB.ByteString where
  mapAPI api request = case decodeNoDup request of
    Left  aesonError -> pure $ badRequestError aesonError
    Right req         -> encode <$> api req

instance MapAPI LT.Text where
  mapAPI api = fmap decodeUtf8 . mapAPI api . encodeUtf8

instance MapAPI ByteString where
  mapAPI api = fmap LB.toStrict . mapAPI api . LB.fromStrict

instance MapAPI Text where
  mapAPI api = fmap LT.toStrict . mapAPI api . LT.fromStrict

renderResponse :: Result e ValidValue -> GQLResponse
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
  = Data ValidValue
  | Errors [GQLError]
  deriving (Show, Generic)

instance FromJSON GQLResponse where
  parseJSON (Aeson.Object hm) = case LH.toList hm of
    [("data"  , value)] -> Data <$> parseJSON value
    [("errors", value)] -> Errors <$> parseJSON value
    _                   -> fail "Invalid GraphQL Response"
  parseJSON _ = fail "Invalid GraphQL Response"

instance ToJSON GQLResponse where
  toJSON (Data gqlData) = object ["data" .= toJSON gqlData]
  toJSON (Errors errors) = object ["errors" .= toJSON errors]
  ----------------------------------------------------------
  toEncoding (Data   _data  ) = pairs $ "data" .= _data
  toEncoding (Errors _errors) = pairs $ "errors" .= _errors