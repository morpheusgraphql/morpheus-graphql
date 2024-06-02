{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.Tag
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
import HConf.Core.Version (Version)
import HConf.Utils.Class (Parse (..))
import Relude hiding (show)

data VersionTag
  = Version Version
  | Latest
  deriving
    ( Generic,
      Eq
    )

instance Parse VersionTag where
  parse = parseText . pack
  parseText "latest" = pure Latest
  parseText s = Version <$> parseText s

instance ToString VersionTag where
  toString Latest = "latest"
  toString (Version v) = toString v

instance Show VersionTag where
  show = toString

instance ToText VersionTag where
  toText = pack . toString

instance FromJSON VersionTag where
  parseJSON (String s) = parseText s
  parseJSON v = Version <$> parseJSON v

instance ToJSON VersionTag where
  toJSON = String . toText

instance Ord VersionTag where
  compare Latest Latest = EQ
  compare Latest Version {} = GT
  compare Version {} Latest = LT
  compare (Version v1) (Version v2) = compare v1 v2
