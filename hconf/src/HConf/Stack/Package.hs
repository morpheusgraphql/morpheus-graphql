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
import Data.Text (unpack)
import qualified HConf.Config.Config as C
import HConf.Config.ConfigT (ConfigT, HCEnv (config), packages)
import HConf.Core.Dependencies (Dependencies)
import HConf.Core.Version (Version)
import HConf.Stack.Cabal (checkCabal)
import HConf.Stack.Lib (Libraries, Library, updateDependencies, updateLibrary)
import HConf.Utils.Core (Name, aesonYAMLOptions, tupled)
import HConf.Utils.Log (label, subTask, task)
import HConf.Utils.Yaml (readYaml, rewriteYaml)
import Relude hiding (Undefined, length, replicate)

data Package = Package
  { name :: Name,
    version :: Version,
    library :: Maybe Library,
    dependencies :: Dependencies,
    tests :: Maybe Libraries,
    executables :: Maybe Libraries,
    benchmarks :: Maybe Libraries
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
resolvePackages = packages >>= traverse (tupled (readYaml . toPath))

updateLibraries :: Maybe Libraries -> ConfigT (Maybe Libraries)
updateLibraries = traverse (traverse updateLibrary)

updatePackage :: Package -> ConfigT Package
updatePackage Package {..} = do
  cfg <- asks config
  newLibrary <- traverse updateLibrary library
  newTests <- updateLibraries tests
  newExecutables <- updateLibraries executables
  newBenchmarks <- updateLibraries benchmarks
  newDependencies <- updateDependencies dependencies
  pure $
    Package
      { version = C.version cfg,
        library = newLibrary,
        tests = newTests,
        executables = newExecutables,
        benchmarks = newBenchmarks,
        dependencies = newDependencies,
        ..
      }

rewritePackage :: Name -> ConfigT Package
rewritePackage path =
  subTask "package" $
    rewriteYaml (toPath path) updatePackage

checkPackage :: Name -> ConfigT ()
checkPackage path =
  task path $ do
    Package {..} <- rewritePackage path
    checkCabal path name version

checkPackages :: ConfigT ()
checkPackages =
  label "packages" $
    packages
      >>= traverse_ checkPackage
