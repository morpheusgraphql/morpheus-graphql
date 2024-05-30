{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.PkgGroup
  ( PkgGroup (..),
    toPackageName,
    isMember,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (toJSON),
    genericToJSON,
  )
import Data.Aeson.Types
  ( defaultOptions,
  )
import Data.List (intercalate)
import Data.Text (isPrefixOf, pack, unpack)
import HConf.Utils.Core (Name)
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )
import System.FilePath.Posix (joinPath, normalise)

data PkgGroup = PkgGroup
  { name :: Name,
    dir :: Maybe FilePath,
    packages :: [Text],
    prefix :: Maybe Bool
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON PkgGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

toPackageName :: PkgGroup -> [Text]
toPackageName PkgGroup {..} = map (pack . pkgPath) packages
  where
    pkgPath pkg =
      let pkgName = intercalate "-" ([unpack name | fromMaybe False prefix] <> [unpack pkg | pkg /= "."])
       in normalise (joinPath (maybeToList dir <> [pkgName]))

isMember :: Name -> PkgGroup -> Bool
isMember pkgName = (`isPrefixOf` pkgName) . name
