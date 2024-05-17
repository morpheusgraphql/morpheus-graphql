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
import HConf.Config (Config, getVersion)
import HConf.ConfigT (ConfigT, packages, withConfig)
import HConf.Lib (Lib, updateDependencies, updateLib)
import HConf.Log (label, task)
import HConf.Utils (Name, tupled)
import HConf.Version (Version)
import HConf.Yaml (Yaml (..), aesonYAMLOptions, mapYaml, mapYamlM, readYaml, rewriteYaml)
import Relude hiding (Undefined, length, replicate)

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

toPath :: Name -> FilePath
toPath = (<> "/package.yaml") . unpack

resolvePackages :: ConfigT [(Name, Package)]
resolvePackages = packages >>= traverse (tupled getPackage)

getPackage :: Name -> ConfigT Package
getPackage = fmap getData . readYaml . toPath

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

checkPackage :: Name -> ConfigT ()
checkPackage name = task name $ rewriteYaml (toPath name) (mapYamlM (withConfig updatePackage))

checkPackages :: ConfigT ()
checkPackages =
  label "packages"
    $ packages
    >>= traverse_ checkPackage