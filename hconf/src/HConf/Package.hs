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
import HConf.Lib (Lib, updateDependencies, updateLib)
import HConf.Version (Version)
import HConf.Yaml (Yaml (..), aesonYAMLOptions, mapYaml, readYaml, writeYaml)
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

resolvePackages :: Config -> IO [(Text, Package)]
resolvePackages config = traverse (\p -> (p,) <$> getPackage p) (getPackages config)

getPackage :: Text -> IO Package
getPackage = fmap getData . readYaml . toPath . unpack

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