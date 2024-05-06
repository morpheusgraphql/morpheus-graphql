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
import Config.Lib (LibType (..))
import Config.Package (Package (..), getPackage)
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

genHie :: Text -> Config -> IO Hie
genHie stack config = do
  packages <- traverse (\p -> (p,) <$> getPackage (unpack p)) (getPackages config)
  pure
    $ Hie
    $ fromList
      [ ( "cradle",
          object
            [ ( "stack",
                object
                  [ ("stackYaml", String stack),
                    ( "components",
                      Array $ fromList $ (concatMap toLib packages)
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

groupComp :: Text -> Text -> Text -> Maybe (KeyMap (Yaml LibType)) -> [Value]
groupComp path name pref (Just libs) = concatMap (\(k, lib) -> toComp path name (Just lib) (pref <> ":" <> K.toText k)) (KM.toList libs)
groupComp _ _ _ _ = []

toLib :: (Text, Package) -> [Value]
toLib (path, Package {..}) =
  toComp path name library "lib"
    <> groupComp path name "test" tests
    <> groupComp path name "exe" executables
    <> groupComp path name "bench" benchmarks
