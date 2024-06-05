{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Class
  ( Parse (..),
    Check (..),
    HConfIO (..),
  )
where

import Control.Exception (tryJust)
import qualified Data.ByteString as L
import Relude

class Parse a where
  parse :: (MonadFail m) => String -> m a
  parseText :: (MonadFail m) => Text -> m a

class Check a where
  check :: (MonadFail m, MonadIO m) => a -> m ()

class (MonadIO m, MonadFail m) => HConfIO m where
  eitherRead :: FilePath -> m (Either String ByteString)
  read :: FilePath -> m ByteString
  write :: FilePath -> ByteString -> m ()

printException :: SomeException -> String
printException = show

instance HConfIO IO where
  eitherRead path = tryJust (Just . printException) (L.readFile path)
  read = L.readFile
  write = L.writeFile
