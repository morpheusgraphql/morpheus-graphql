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
module Config.Hie
  ( genHie,
  )
where

import Config.File
import Config.Package (LibType (..), PackageType (..), readPackage)
import Config.Types (Config, getPackages)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object)
import qualified Data.Aeson.Key as K
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (unpack)
import Relude hiding (Undefined, intercalate)

newtype Hie = Hie (KeyMap Value)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Show
    )

genHie :: [Text] -> Text -> Config -> IO Hie
genHie extra stack config = do
  packages <- traverse (\p -> (p,) . getData <$> readPackage (unpack p)) (getPackages config <> extra)
  pure
    $ Hie
    $ fromList
      [ ( "cradle",
          object
            [ ( "stack",
                object
                  [ ("stackYaml", String stack),
                    ( "components",
                      Array
                        $ fromList
                        $ ( concatMap toLib packages
                              <> concatMap toTests packages
                              <> concatMap toExec packages
                              <> concatMap toBench packages
                          )
                    )
                  ]
              )
            ]
        )
      ]

toComp :: Text -> Text -> Maybe (Yaml LibType) -> Text -> [Value]
toComp path name (Just (Yaml LibType {..} _)) lib =
  [ object
      [ ("path", String ("./" <> path <> "/" <> sourceDirs)),
        ("component", String (name <> ":" <> lib))
      ]
  ]
toComp _ _ _ _ = []

groupComp :: Text -> Text -> Text -> KeyMap (Yaml LibType) -> [Value]
groupComp path name pref libs = concatMap (\(k, lib) -> toComp path name (Just lib) (pref <> ":" <> K.toText k)) (KM.toList libs)

toLib :: (Text, PackageType) -> [Value]
toLib (path, PackageType {library, name}) = toComp path name library "lib"

toTests :: (Text, PackageType) -> [Value]
toTests (path, PackageType {tests = Just m, name}) = groupComp path name "test" m
toTests _ = []

toExec :: (Text, PackageType) -> [Value]
toExec (path, PackageType {executables = Just m, name}) = groupComp path name "exe" m
toExec _ = []

toBench :: (Text, PackageType) -> [Value]
toBench (path, PackageType {benchmarks = Just m, name}) = groupComp path name "bench" m
toBench _ = []
