{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Config
  ( Service (..),
    Config (..),
    readConfig,
    ServiceOptions (..),
    Source (..),
  )
where

import Config.Types (Config)
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

data ServiceOptions = ServiceOptions
  { optionNamespace :: Bool,
    optionImports :: [Text],
    optionExternals :: HashMap Text Text
  }
  deriving (Show)

instance FromJSON ServiceOptions where
  parseJSON =
    withObject
      "ServiceOptions"
      ( \v -> do
          optionNamespace <- fromMaybe False <$> v .:? "namespace"
          optionExternals <- collection "externals" v
          optionImports <- collection "globals" v
          pure ServiceOptions {..}
      )
    where
      collection name v = fromMaybe mempty <$> (v .:? name)

instance Semigroup ServiceOptions where
  (ServiceOptions n1 g1 e1) <> (ServiceOptions n2 g2 e2) = ServiceOptions (n1 || n2) (g1 <> g2) (e1 <> e2)

data Source = Source
  { sourcePath :: FilePath,
    sourceOptions :: Maybe ServiceOptions
  }
  deriving (Show)

readConfig :: FilePath -> IO Config
readConfig path = do
  file <- L.readFile (path </> "code-gen.yaml")
  decodeThrow file
