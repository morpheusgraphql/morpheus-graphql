{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Package
  ( Package (..),
    checkPackages,
    resolvePackages,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Text (unpack)
import HConf.Config (Config, getPackages, getVersion)
import HConf.ConfigT (ConfigT, HCEnv (config))
import HConf.Lib (Lib, updateDependencies, updateLib)
import HConf.Utils (Name, tupled)
import HConf.Version (Version)
import HConf.Yaml (Yaml (..), aesonYAMLOptions, mapYaml, readYaml, writeYaml)
import Relude hiding (Undefined, intercalate, length, replicate)

type Libs = Maybe (KeyMap Lib)

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

resolvePackages :: ConfigT [(Text, Package)]
resolvePackages = asks config >>= traverse (tupled getPackage) . getPackages

getPackage :: Text -> ConfigT Package
getPackage = fmap getData . readYaml . toPath . unpack

updateDeps :: Config -> Libs -> Libs
updateDeps config = fmap (fmap (mapYaml (updateLib config)))

updatePackage :: Config -> Package -> Package
updatePackage config Package {..} =
  Package
    { version = getVersion config,
      library = fmap (mapYaml (updateLib config)) library,
      tests = updateDeps config tests,
      executables = updateDeps config executables,
      benchmarks = updateDeps config benchmarks,
      dependencies = updateDependencies config dependencies,
      ..
    }

checkPackage :: FilePath -> ConfigT ()
checkPackage path = do
  pkg <- readYaml path
  cfg <- asks config
  writeYaml path (mapYaml (updatePackage cfg) pkg)

checkPackages :: ConfigT ()
checkPackages = asks config >>= traverse_ (checkPackage . toPath . unpack) . getPackages