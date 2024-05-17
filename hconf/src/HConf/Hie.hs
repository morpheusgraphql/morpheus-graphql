{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Hie
  ( genHie,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object)
import qualified Data.Aeson.Key as K
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import HConf.ConfigT (ConfigT, HCEnv (..))
import HConf.Env (Env (..))
import HConf.Lib (Lib, LibType (..))
import HConf.Log (label, listItem)
import HConf.Package (Package (..), resolvePackages)
import HConf.Yaml (Yaml (..), writeYaml)
import Relude hiding (Undefined, intercalate)

data Component = Component
  { path :: Text,
    component :: Text
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show
    )

data Components = Components
  { stackYaml :: FilePath,
    components :: [Component]
  }
  deriving
    ( ToJSON,
      FromJSON,
      Generic,
      Show
    )

packHie :: Components -> Value
packHie value = (object [("cradle", object [("stack", toJSON value)])])

(<:>) :: (Semigroup a, IsString a) => a -> a -> a
(<:>) name tag = name <> ":" <> tag

toLib :: (Text, Package) -> [Component]
toLib (path, Package {..}) =
  comp "lib" library
    <> compGroup "test" tests
    <> compGroup "exe" executables
    <> compGroup "bench" benchmarks
  where
    compGroup :: Text -> Maybe (KeyMap Lib) -> [Component]
    compGroup tag = concatMap mkComp . concatMap KM.toList . maybeToList
      where
        mkComp (k, lib) = comp (tag <:> K.toText k) (Just lib)
    comp :: Text -> Maybe Lib -> [Component]
    comp tag (Just (Yaml LibType {sourceDirs} _)) =
      [ Component
          { path = "./" <> path <> "/" <> sourceDirs,
            component = name <:> tag
          }
      ]
    comp _ _ = []

genHie :: ConfigT ()
genHie = label "hie" $ listItem ("hie.yaml" :: String) $ do
  Env {..} <- asks env
  components <- concatMap toLib <$> resolvePackages
  writeYaml hie (packHie Components {stackYaml = stack, components})
