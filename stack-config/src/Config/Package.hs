{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Package
  ( Package (..),
    readPackage,
    checkPackages,
  )
where

import Config.File (Yaml (..), aesonYAMLOptions, mapYaml, readYaml, writeYaml)
import Config.Lib (Lib, updateDependencies, updateLib)
import Config.Types (Config, getPackages, getVersion)
import Config.Version (Version)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Text (unpack)
import Relude hiding (Undefined, intercalate, length, replicate)

type Libs = Maybe (KeyMap Lib)

type Name = Text

data Package = Package
  { name :: Name,
    version :: Version,
    library :: Maybe Lib,
    dependencies :: [Name],
    tests :: Libs,
    executables :: Libs,
    benchmarks :: Libs
  }
  deriving
    ( Show,
      Generic
    )

instance FromJSON Package where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON Package where
  toJSON = genericToJSON aesonYAMLOptions

toPath :: FilePath -> FilePath
toPath = (<> "/package.yaml")

readPackage :: FilePath -> IO Package
readPackage = fmap getData . readYaml . toPath

updateDeps :: Config -> Libs -> Libs
updateDeps config = fmap (fmap (updateLib config))

updatePackage :: Config -> Package -> Package
updatePackage config Package {..} =
  Package
    { version = getVersion config,
      library = fmap (updateLib config) library,
      tests = updateDeps config tests,
      executables = updateDeps config executables,
      benchmarks = updateDeps config benchmarks,
      dependencies = updateDependencies config dependencies,
      ..
    }

checkPackage :: Config -> FilePath -> IO ()
checkPackage config path = readYaml path >>= writeYaml path . mapYaml (updatePackage config)

checkPackages :: Config -> IO ()
checkPackages config = traverse_ (checkPackage config . toPath . unpack) (getPackages config)