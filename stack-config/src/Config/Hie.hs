{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Hie
  ( genHie,
  )
where

import Config.File
import Config.Package (Package, PackageType (..), readPackage)
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
  packages <- traverse (readPackage . unpack) (getPackages config)
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

toComponent :: Package -> [Value]
toComponent (Yaml PackageType {..} _) =
  [ object
      [ ("path", String name),
        ("component", String "")
      ]
  ]