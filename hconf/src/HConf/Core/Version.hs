{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Version
  ( Version (..),
    nextVersion,
    dropPatch,
    fetchVersions,
    VersionNumber,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Map (lookup)
import Data.Text
  ( pack,
    split,
    unpack,
  )
import GHC.Show (Show (show))
import HConf.Utils.Class (Parse (..))
import HConf.Utils.Http (hackage)
import Relude hiding
  ( Undefined,
    break,
    drop,
    fromList,
    isPrefixOf,
    length,
    null,
    show,
    toList,
  )

newtype VersionNumber = VersionNumber [Int]
  deriving
    ( Generic,
      Eq
    )

nextVersion :: (MonadFail m) => Bool -> VersionNumber -> m VersionNumber
nextVersion isBreaking (VersionNumber [major, minor, revision])
  | isBreaking = pure $ VersionNumber [major, minor + 1, 0]
  | otherwise = pure $ VersionNumber [major, minor, revision + 1]
nextVersion _ v = fail $ "can't update version " <> show v

dropPatch :: VersionNumber -> VersionNumber
dropPatch (VersionNumber xs) = VersionNumber (take 2 xs <> [0])

compareSeries :: (Ord a) => [a] -> [a] -> Ordering
compareSeries [] _ = EQ
compareSeries _ [] = EQ
compareSeries (x : xs) (y : ys)
  | x == y = compareSeries xs ys
  | otherwise = compare x y

instance Parse VersionNumber where
  parse = parseText . pack
  parseText s =
    maybe
      (fail $ "invalid version: '" <> toString s <> "'!")
      (pure . VersionNumber)
      $ traverse (readMaybe . unpack)
      $ split (== '.') s

instance ToString VersionNumber where
  toString (VersionNumber ns) = intercalate "." $ map show ns

instance Ord VersionNumber where
  compare (VersionNumber v1) (VersionNumber v2) = compareSeries v1 v2

instance Show VersionNumber where
  show = toString

instance ToText VersionNumber where
  toText = pack . toString

instance FromJSON VersionNumber where
  parseJSON (String s) = parseText s
  parseJSON (Number n) = parse (show n)
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON VersionNumber where
  toJSON = String . toText

data Version
  = Version VersionNumber
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

instance Parse Version where
  parse = parseText . pack
  parseText "latest" = pure LatestVersion
  parseText s = Version <$> parseText s

instance ToString Version where
  toString LatestVersion = "latest"
  toString (Version v) = toString v

instance Show Version where
  show = toString

instance ToText Version where
  toText = pack . toString

instance FromJSON Version where
  parseJSON (String s) = parseText s
  parseJSON (Number n) = parse (show n)
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Version where
  toJSON = String . toText

instance Ord Version where
  compare LatestVersion LatestVersion = EQ
  compare LatestVersion (Version _) = GT
  compare (Version _) LatestVersion = LT
  compare (Version v1) (Version v2) = compare v1 v2

fetchVersionResponse :: (MonadIO m, MonadFail m) => String -> m (Either String (Map Text (NonEmpty VersionNumber)))
fetchVersionResponse name = hackage ["package", name, "preferred"]

lookupVersions :: (MonadFail m) => Either String (Map Text (NonEmpty VersionNumber)) -> m (NonEmpty VersionNumber)
lookupVersions (Right x) = maybe (fail "field normal-version not found") pure (lookup "normal-version" x)
lookupVersions (Left x) = fail x

fetchVersions :: (MonadFail m, MonadIO m) => String -> m (NonEmpty VersionNumber)
fetchVersions name = fetchVersionResponse name >>= lookupVersions