{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config (Config (..), PkgGroup (..), parseYaml, serializeYaml) where

import Config.Types
import Data.Aeson (FromJSON, ToJSON)
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import Relude

parseYaml :: (FromJSON a) => ByteString -> IO a
parseYaml = decodeThrow

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml = encodePretty (setConfDropNull True $ setConfCompare compareFields defConfig)
