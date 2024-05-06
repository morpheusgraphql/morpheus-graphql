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