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

newtype ServiceOptions = ServiceOptions
  { namespace :: Bool
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

data Service = Service
  { name :: Text,
    includes :: [Text],
    source :: Text,
    options :: Maybe ServiceOptions
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

data Config = Config
  { server :: [Service],
    client :: [Service]
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
