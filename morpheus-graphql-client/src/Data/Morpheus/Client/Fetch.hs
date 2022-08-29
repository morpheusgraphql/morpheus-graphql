{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch
  ( Fetch (..),
    encodeRequest,
    decodeResponse,
    Request (..),
    RequestType (..),
    Response,
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
    METHOD (..),
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
    Text,
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

encodeRequest :: forall m s a. (RequestType a, ToJSON (Args a)) => Request s m a -> ByteString
encodeRequest req =
  encode
    ( GQLRequest
        { operationName = Just (__name (Proxy @a)),
          query = pack (__query (Proxy @a)),
          variables = fixVars (toJSON (getRequestArgs req))
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
  fetch f args = decodeResponse <$> f (encodeRequest request)
    where
      request :: Request 'HTTP m a
      request = HttpRequest args ""

class RequestType a where
  type RequestMethod a :: METHOD
  type RequestArgs a :: Type
  __name :: Proxy a -> FieldName
  __query :: Proxy a -> String

data Request (method :: METHOD) m (a :: Type) where
  HttpRequest ::
    { requestArgs :: Args a,
      httpEndpoint :: Text
    } ->
    Request 'HTTP m a
  WSSubscription ::
    { subscriptionArgs :: Args a,
      wsEndpoint :: Text,
      subscriptionHandler :: ClientResult a -> m ()
    } ->
    Request 'WS m a

getRequestArgs :: Request stream m a -> Args a
getRequestArgs HttpRequest {requestArgs} = requestArgs
getRequestArgs WSSubscription {subscriptionArgs} = subscriptionArgs

type family Response method m a where
  Response 'WS m a = m ()
  Response 'HTTP m a = m (ClientResult a)