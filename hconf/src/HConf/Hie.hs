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
import Data.Text (unpack)
import HConf.Config (Config, getPackages)
import HConf.Lib (LibType (..))
import HConf.Package (Package (..), getPackage)
import HConf.Yaml (Yaml (..), writeYaml)
import Relude hiding (Undefined, intercalate)

newtype Hie = Hie (KeyMap Value)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Show
    )

genHie :: FilePath -> Text -> Config -> IO ()
genHie hiePath stack config =
  do
    packages <- traverse (\p -> (p,) <$> getPackage (unpack p)) (getPackages config)
    let hie =
          Hie
            ( fromList
                [ ( "cradle",
                    object
                      [ ( "stack",
                          object
                            [ ("stackYaml", String stack),
                              ( "components",
                                toJSON (concatMap toLib packages)
                              )
                            ]
                        )
                      ]
                  )
                ]
            )
    writeYaml hiePath hie

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

mkComponent :: Text -> Text -> Text -> Maybe (Yaml LibType) -> [Component]
mkComponent path name libTag (Just (Yaml LibType {..} _)) =
  [ Component
      { path = "./" <> path <> "/" <> sourceDirs,
        component = name <> ":" <> libTag
      }
  ]
mkComponent _ _ _ _ = []

toLib :: (Text, Package) -> [Component]
toLib (path, Package {..}) =
  comp "lib" library
    <> compGroup "test" tests
    <> compGroup "exe" executables
    <> compGroup "bench" benchmarks
  where
    compGroup :: Text -> Maybe (KeyMap (Yaml LibType)) -> [Component]
    compGroup tag (Just libs) = concatMap mkComp (KM.toList libs)
      where
        mkComp (k, lib) = comp (tag <> ":" <> K.toText k) (Just lib)
    compGroup _ _ = []
    comp = mkComponent path name