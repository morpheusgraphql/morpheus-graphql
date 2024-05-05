{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Package
  ( readPackage,
    PackageType (..),
    Package,
    LibType (..),
    checkPackages,
  )
where

import Config.File (Yaml, aesonYAMLOptions, readYaml, writeYaml)
import Config.Types (Config, getPackages)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Text (unpack)
import Relude hiding (Undefined, intercalate)

type Package = Yaml PackageType

data LibType = LibType
  { sourceDirs :: Text,
    dependencies :: Maybe [Text]
  }
  deriving
    ( Show,
      Generic
    )

type Lib = Yaml LibType

instance FromJSON LibType where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON LibType where
  toJSON = genericToJSON aesonYAMLOptions

data PackageType = PackageType
  { name :: Text,
    library :: Maybe Lib,
    tests :: Maybe (KeyMap Lib),
    executables :: Maybe (KeyMap Lib),
    benchmarks :: Maybe (KeyMap Lib)
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON PackageType where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON PackageType where
  toJSON = genericToJSON aesonYAMLOptions

readPackage :: FilePath -> IO Package
readPackage file = readYaml (file <> "/package.yaml")

updatePackage :: Config -> PackageType -> PackageType
updatePackage _ = id

checkPackage :: Config -> Text -> IO ()
checkPackage config path = readPackage (unpack path) >>= writeYaml (unpack path <> "/package.yaml") . updatePackage config

checkPackages :: Config -> IO ()
checkPackages config = traverse_ (checkPackage config) (getPackages config)