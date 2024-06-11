{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch
  ( Fetch (..),
    decodeResponse,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
    eitherDecode,
    encode,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Client.Fetch.RequestType (Request (Request), RequestType (RequestArgs), processResponse, toRequest)
import Data.Morpheus.Client.Fetch.Types
  ( FetchError (..),
  )
import Relude hiding (ByteString)

decodeResponse :: (FromJSON a) => ByteString -> Either (FetchError a) a
decodeResponse = (first FetchErrorParseFailure . eitherDecode) >=> processResponse

class (RequestType a, ToJSON (Args a), FromJSON a) => Fetch a where
  type Args a :: Type
  fetch :: (Monad m) => (ByteString -> m ByteString) -> Args a -> m (Either (FetchError a) a)

instance (RequestType a, ToJSON (Args a), FromJSON a) => Fetch a where
  type Args a = RequestArgs a
  fetch f args = decodeResponse <$> f (encode $ toRequest request)
    where
      request :: Request a
      request = Request args
