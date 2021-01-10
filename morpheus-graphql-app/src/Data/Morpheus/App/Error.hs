{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Error
  ( badRequestError,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import Relude hiding (ByteString)

badRequestError :: String -> ByteString
badRequestError = pack . ("Bad Request. Could not decode Request body: " <>)
