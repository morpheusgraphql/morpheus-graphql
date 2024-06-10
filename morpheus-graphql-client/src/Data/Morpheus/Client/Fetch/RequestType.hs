{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch.RequestType
  ( toRequest,
    decodeResponse,
    Request (..),
    RequestType (..),
    processResponse,
    ClientTypeConstraint,
    isSubscription,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
    eitherDecode,
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Client.Fetch.Types
  ( FetchError (..),
  )
import Data.Morpheus.Client.Schema.JSON.Types
  ( JSONResponse (..),
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    OperationType (..),
  )
import Data.Text
  ( pack,
  )
import Relude hiding (ByteString)

fixVars :: A.Value -> Maybe A.Value
fixVars x
  | x == A.emptyArray = Nothing
  | otherwise = Just x

toRequest :: (RequestType a, ToJSON (RequestArgs a)) => Request a -> GQLRequest
toRequest r@Request {requestArgs} =
  ( GQLRequest
      { operationName = Just (__name r),
        query = pack (__query r),
        variables = fixVars (toJSON requestArgs)
      }
  )

decodeResponse :: (FromJSON a) => ByteString -> Either (FetchError a) a
decodeResponse = (first FetchErrorParseFailure . eitherDecode) >=> processResponse

processResponse :: JSONResponse a -> Either (FetchError a) a
processResponse JSONResponse {responseData = Just x, responseErrors = []} = Right x
processResponse JSONResponse {responseData = Nothing, responseErrors = []} = Left FetchErrorNoResult
processResponse JSONResponse {responseData = result, responseErrors = (x : xs)} = Left $ FetchErrorProducedErrors (x :| xs) result

type ClientTypeConstraint (a :: Type) = (RequestType a, ToJSON (RequestArgs a), FromJSON a)

class RequestType a where
  type RequestArgs a :: Type
  __name :: f a -> FieldName
  __query :: f a -> String
  __type :: f a -> OperationType

newtype Request (a :: Type) = Request {requestArgs :: RequestArgs a}

isSubscription :: (RequestType a) => Request a -> Bool
isSubscription x = __type x == OPERATION_SUBSCRIPTION
