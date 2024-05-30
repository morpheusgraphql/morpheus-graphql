{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Stack.Package
  ( Package (..),
    checkPackages,
    resolvePackages,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Text (unpack)
import HConf.Config.Config (getVersion)
import HConf.Config.ConfigT (ConfigT, HCEnv (config), packages)
import HConf.Core.Dependencies (Dependencies)
import HConf.Core.Version (Version)
import HConf.Stack.Lib (Library, updateDependencies, updateLib)
import HConf.Utils.Core (Name, aesonYAMLOptions, tupled)
import HConf.Utils.Log (label, task)
import HConf.Yaml (readYaml, rewriteYaml)
import Relude hiding (Undefined, length, replicate)

type Libs = Maybe (KeyMap Library)

data Package = Package
  { name :: Name,
    version :: Version,
    library :: Maybe Library,
    dependencies :: Dependencies,
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

toPath :: Name -> FilePath
toPath = (<> "/package.yaml") . unpack

resolvePackages :: ConfigT [(Name, Package)]
resolvePackages = packages >>= traverse (tupled getPackage)

getPackage :: Name -> ConfigT Package
getPackage = readYaml . toPath

updateDeps :: Libs -> ConfigT Libs
updateDeps = traverse (traverse updateLib)

updatePackage :: Package -> ConfigT Package
updatePackage Package {..} = do
  cfg <- asks config
  newLibrary <- traverse updateLib library
  newTests <- updateDeps tests
  newExecutables <- updateDeps executables
  newBenchmarks <- updateDeps benchmarks
  newDependencies <- updateDependencies dependencies
  pure
    $ Package
      { version = getVersion cfg,
        library = newLibrary,
        tests = newTests,
        executables = newExecutables,
        benchmarks = newBenchmarks,
        dependencies = newDependencies,
        ..
      }

checkPackage :: Name -> ConfigT ()
checkPackage name = task name $ rewriteYaml (toPath name) updatePackage

checkPackages :: ConfigT ()
checkPackages = label "packages" $ packages >>= traverse_ checkPackage
