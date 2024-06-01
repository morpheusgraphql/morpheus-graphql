{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Hie
  ( genHie,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object)
import qualified Data.Map as M
import HConf.Config.ConfigT (ConfigT, HCEnv (..))
import HConf.Core.Env (Env (..))
import HConf.Stack.Lib (Libraries, Library (..))
import HConf.Stack.Package (Package (..), resolvePackages)
import HConf.Utils.Log (label, task)
import HConf.Utils.Yaml (writeYaml)
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
packHie value = object [("cradle", object [("stack", toJSON value)])]

(<:>) :: (Semigroup a, IsString a) => a -> a -> a
(<:>) name tag = name <> ":" <> tag

toLib :: (Text, Package) -> [Component]
toLib (path, Package {..}) =
  comp "lib" library
    <> compGroup "test" tests
    <> compGroup "exe" executables
    <> compGroup "bench" benchmarks
  where
    compGroup :: Text -> Maybe Libraries -> [Component]
    compGroup tag = concatMap mkComp . concatMap M.toList . maybeToList
      where
        mkComp (k, lib) = comp (tag <:> k) (Just lib)
    comp :: Text -> Maybe Library -> [Component]
    comp tag (Just Library {sourceDirs}) =
      [ Component
          { path = "./" <> path <> "/" <> sourceDirs,
            component = name <:> tag
          }
      ]
    comp _ _ = []

genHie :: ConfigT ()
genHie = label "hie" $
  task "hie.yaml" $ do
    Env {..} <- asks env
    components <- concatMap toLib <$> resolvePackages
    writeYaml hie (packHie Components {stackYaml = stack, components})
