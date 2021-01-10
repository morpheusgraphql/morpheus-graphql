{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse (..),
    MapAPI (..),
  )
where

import Data.Aeson
  ( encode,
  )
import Data.Aeson.Internal
  ( formatError,
    ifromJSON,
  )
import Data.Aeson.Parser
  ( eitherDecodeWith,
    jsonNoDup,
  )
import qualified Data.ByteString.Lazy.Char8 as LB
  ( ByteString,
    fromStrict,
    toStrict,
  )
import Data.Morpheus.App.Error (badRequestError)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.IO
  ( GQLRequest (..),
    GQLResponse (..),
  )
import qualified Data.Text.Lazy as LT
  ( Text,
    fromStrict,
    toStrict,
  )
import Data.Text.Lazy.Encoding
  ( decodeUtf8,
    encodeUtf8,
  )
import Relude hiding
  ( decodeUtf8,
    encodeUtf8,
  )

decodeNoDup :: Failure String m => LB.ByteString -> m GQLRequest
decodeNoDup str = case eitherDecodeWith jsonNoDup ifromJSON str of
  Left (path, x) -> failure $ formatError path x
  Right value -> pure value

class MapAPI a b where
  mapAPI :: Applicative m => (GQLRequest -> m GQLResponse) -> a -> m b

instance MapAPI GQLRequest GQLResponse where
  mapAPI f = f

instance MapAPI LB.ByteString LB.ByteString where
  mapAPI api request = case decodeNoDup request of
    Left aesonError -> pure $ badRequestError aesonError
    Right req -> encode <$> api req

instance MapAPI LT.Text LT.Text where
  mapAPI api = fmap decodeUtf8 . mapAPI api . encodeUtf8

instance MapAPI ByteString ByteString where
  mapAPI api = fmap LB.toStrict . mapAPI api . LB.fromStrict

instance MapAPI Text Text where
  mapAPI api = fmap LT.toStrict . mapAPI api . LT.fromStrict
