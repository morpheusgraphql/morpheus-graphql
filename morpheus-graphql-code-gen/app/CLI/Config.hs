{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Config
  ( Service (..),
    Config (..),
    readConfig,
    ServiceOptions (..),
  )
where

import qualified Data.ByteString as L
  ( readFile,
  )
import Data.Yaml (FromJSON, decodeThrow)
import Relude
import System.FilePath.Posix
  ( (</>),
  )

data ServiceOptions = ServiceOptions
  { namespace :: Maybe Bool,
    globals :: Maybe [Text]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

data Service = Service
  { name :: String,
    includes :: [FilePath],
    source :: FilePath,
    options :: Maybe ServiceOptions,
    schema :: Maybe FilePath
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

data Config = Config
  { server :: Maybe [Service],
    client :: Maybe [Service]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

readConfig :: FilePath -> IO Config
readConfig path = do
  file <- L.readFile (path </> "code-gen.yaml")
  decodeThrow file
