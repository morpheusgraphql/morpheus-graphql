{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Config
  ( Config (..),
    readConfig,
  )
where

import Config (Config (..))
import qualified Data.ByteString as L
  ( readFile,
  )
import Data.Yaml
  ( FromJSON (..),
    Value (..),
    decodeThrow,
    withObject,
    (.:),
    (.:?),
  )
import Relude
import System.FilePath.Posix
  ( (</>),
  )

readConfig :: FilePath -> IO Config
readConfig path = do
  file <- L.readFile "./config/stack.yaml"
  decodeThrow file
