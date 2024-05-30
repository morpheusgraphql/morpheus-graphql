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
import HConf.Utils (Name)
import Relude hiding
  ( Undefined,
    group,
    intercalate,
    isPrefixOf,
  )
import System.FilePath.Posix (joinPath, normalise)

data PkgGroup = PkgGroup
  { group :: Name,
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
    pkgPath name =
      let pkgName = intercalate "-" ([unpack group | fromMaybe False prefix] <> [unpack name | name /= "."])
       in normalise (joinPath (maybeToList dir <> [pkgName]))

isMember :: Name -> PkgGroup -> Bool
isMember name = (`isPrefixOf` name) . group
