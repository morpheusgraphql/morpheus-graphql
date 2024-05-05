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
import Config.Package (LibType (..), Package, PackageType (..), readPackage)
import Config.Types (Config, getPackages)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object)
import Data.Aeson.KeyMap (KeyMap)
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
  packages <- traverse (\p -> (p,) <$> readPackage (unpack p)) (getPackages config)
  pure
    $ Hie
    $ fromList
      [ ( "cradle",
          object
            [ ( "stack",
                object
                  [ ("stackYaml", String stack),
                    ("components", Array $ fromList $ concatMap toComponent packages)
                  ]
              )
            ]
        )
      ]

toComponent :: (Text, Package) -> [Value]
toComponent (path, Yaml PackageType {library = Just (Yaml LibType {..} _), name} _) =
  [ object
      [ ("path", String ("./" <> path <> "/" <> sourceDirs)),
        ("component", String (name <> ":lib"))
      ]
  ]
toComponent _ = []