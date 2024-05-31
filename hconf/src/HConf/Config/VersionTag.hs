{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.VersionTag
  ( VersionTag (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Text (pack)
import GHC.Show (Show (show))
import HConf.Core.Version (VersionNumber)
import HConf.Utils.Class (Parse (..))
import Relude hiding (show)

data VersionTag
  = Version VersionNumber
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

instance Parse VersionTag where
  parse = parseText . pack
  parseText "latest" = pure LatestVersion
  parseText s = Version <$> parseText s

instance ToString VersionTag where
  toString LatestVersion = "latest"
  toString (Version v) = toString v

instance Show VersionTag where
  show = toString

instance ToText VersionTag where
  toText = pack . toString

instance FromJSON VersionTag where
  parseJSON (String s) = parseText s
  parseJSON (Number n) = parse (show n)
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON VersionTag where
  toJSON = String . toText

instance Ord VersionTag where
  compare LatestVersion LatestVersion = EQ
  compare LatestVersion (Version _) = GT
  compare (Version _) LatestVersion = LT
  compare (Version v1) (Version v2) = compare v1 v2
