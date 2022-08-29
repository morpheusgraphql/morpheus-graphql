{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch
  ( Fetch (..),
    toRequest,
    decodeResponse,
    Request (..),
    RequestType (..),
    Response,
    processResponse,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
    eitherDecode,
    encode,
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Client.Internal.Types
  ( ClientResult,
    FetchError (..),
    HTTP,
    METHOD (..),
    WS,
  )
import Data.Morpheus.Client.Schema.JSON.Types
  ( JSONResponse (..),
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
  )
import Data.Text
  ( pack,
  )
import Relude
  ( Bifunctor (first),
    Either (..),
    Eq ((==)),
    Maybe (..),
    Monad,
    NonEmpty ((:|)),
    Proxy (..),
    String,
    Type,
    otherwise,
    ($),
    (.),
    (<$>),
    (>=>),
  )

fixVars :: A.Value -> Maybe A.Value
fixVars x
  | x == A.emptyArray = Nothing
  | otherwise = Just x

toRequest :: forall s a. (RequestType a, ToJSON (Args a)) => Request s a -> GQLRequest
toRequest Request {requestArgs} =
  ( GQLRequest
      { operationName = Just (__name (Proxy @a)),
        query = pack (__query (Proxy @a)),
        variables = fixVars (toJSON requestArgs)
      }
  )

decodeResponse :: FromJSON a => ByteString -> Either (FetchError a) a
decodeResponse = (first FetchErrorParseFailure . eitherDecode) >=> processResponse

processResponse :: JSONResponse a -> Either (FetchError a) a
processResponse JSONResponse {responseData = Just x, responseErrors = []} = Right x
processResponse JSONResponse {responseData = Nothing, responseErrors = []} = Left FetchErrorNoResult
processResponse JSONResponse {responseData = result, responseErrors = (x : xs)} = Left $ FetchErrorProducedErrors (x :| xs) result

class (RequestType a, ToJSON (Args a), FromJSON a) => Fetch a where
  type Args a :: Type
  fetch :: Monad m => (ByteString -> m ByteString) -> Args a -> m (Either (FetchError a) a)

instance (RequestType a, ToJSON (Args a), FromJSON a) => Fetch a where
  type Args a = RequestArgs a
  fetch f args = decodeResponse <$> f (encode $ toRequest request)
    where
      request :: Request HTTP a
      request = Request args

class RequestType a where
  type RequestMethod a :: METHOD
  type RequestArgs a :: Type
  __name :: Proxy a -> FieldName
  __query :: Proxy a -> String

newtype Request (method :: METHOD) (a :: Type) = Request {requestArgs :: Args a}

type family Response method m a where
  Response WS m a = m ()
  Response HTTP m a = m (ClientResult a)
