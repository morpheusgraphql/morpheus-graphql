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

import Config.File (Yaml (..), aesonYAMLOptions, readYaml, writeYaml)
import Config.Types (Config, getPackages, getVersion)
import Config.Version (Version)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Text (unpack)
import Relude hiding (Undefined, intercalate)

type Package = Yaml PackageType

type Dep = Maybe [Text]

data LibType = LibType
  { sourceDirs :: Text
  -- dependencies :: Maybe [Text]
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

type Libs = Maybe (KeyMap Lib)

data PackageType = PackageType
  { name :: Text,
    version :: Version,
    library :: Maybe Lib,
    dependencies :: [Text],
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

updatePackage :: Config -> Package -> Package
updatePackage config (Yaml v props) =
  ( Yaml
      ( v
          { version = getVersion config,
            library = fmap (updateLib config) (library v),
            tests = updateDeps config (tests v),
            executables = updateDeps config (executables v),
            benchmarks = updateDeps config (benchmarks v),
            dependencies = updateDep config (dependencies v)
          }
      )
      props
  )

updateDep :: Config -> [Text] -> [Text]
updateDep _ l = traceShow l l

updateLib :: Config -> Lib -> Lib
updateLib _ l = l

updateDeps :: Config -> Libs -> Libs
updateDeps config (Just x) = Just (fmap (updateLib config) x)
updateDeps _ Nothing = Nothing

checkPackage :: Config -> Text -> IO ()
checkPackage config path =
  readPackage (unpack path)
    >>= writeYaml (toPath $ unpack path)
    . updatePackage config

checkPackages :: Config -> IO ()
checkPackages config = traverse_ (checkPackage config) (getPackages config)