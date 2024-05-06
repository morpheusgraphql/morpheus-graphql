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
import Data.Aeson.Types (listValue)
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
                                listValue id $ (concatMap toLib packages)
                              )
                            ]
                        )
                      ]
                  )
                ]
            )
    writeYaml hiePath hie

component :: Text -> Text -> Text -> Maybe (Yaml LibType) -> [Value]
component path name lib (Just (Yaml LibType {..} _)) =
  [ object
      [ ("path", String ("./" <> path <> "/" <> sourceDirs)),
        ("component", String (name <> ":" <> lib))
      ]
  ]
component _ _ _ _ = []

toLib :: (Text, Package) -> [Value]
toLib (path, Package {..}) =
  comp "lib" library
    <> compGroup "test" tests
    <> compGroup "exe" executables
    <> compGroup "bench" benchmarks
  where
    compGroup :: Text -> Maybe (KeyMap (Yaml LibType)) -> [Value]
    compGroup pref (Just libs) = concatMap (\(k, lib) -> comp (pref <> ":" <> K.toText k) (Just lib)) (KM.toList libs)
    compGroup _ _ = []
    comp = component path name