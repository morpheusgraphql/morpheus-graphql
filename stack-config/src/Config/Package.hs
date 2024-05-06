{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Package
  ( PackageType (..),
    Package,
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

type Package = Yaml PackageType

type Libs = Maybe (KeyMap Lib)

type Name = Text

data PackageType = PackageType
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

instance FromJSON PackageType where
  parseJSON = genericParseJSON aesonYAMLOptions

instance ToJSON PackageType where
  toJSON = genericToJSON aesonYAMLOptions

toPath :: FilePath -> FilePath
toPath = (<> "/package.yaml")

readPackage :: FilePath -> IO Package
readPackage = readYaml . toPath

writePackage :: FilePath -> Package -> IO ()
writePackage path = writeYaml (toPath path)

updateDeps :: Config -> Libs -> Libs
updateDeps config = fmap (fmap (updateLib config))

updatePackage :: Config -> PackageType -> PackageType
updatePackage config PackageType {..} =
  PackageType
    { version = getVersion config,
      library = fmap (updateLib config) library,
      tests = updateDeps config tests,
      executables = updateDeps config executables,
      benchmarks = updateDeps config benchmarks,
      dependencies = updateDependencies config dependencies,
      ..
    }

checkPackage :: Config -> FilePath -> IO ()
checkPackage config path = readPackage path >>= writePackage path . mapYaml (updatePackage config)

checkPackages :: Config -> IO ()
checkPackages config = traverse_ (checkPackage config . unpack) (getPackages config)