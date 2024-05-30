{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Version
  ( Version (..),
    nextVersion,
    dropPatch,
    fetchVersions,
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
import HConf.Utils.Http (hackage)
import HConf.Utils.Parse (Parse (..))
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

data Version
  = Version [Int]
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

dropPatch :: Version -> Version
dropPatch (Version xs) = Version (take 2 xs <> [0])
dropPatch LatestVersion = LatestVersion

nextVersion :: (MonadFail m) => Bool -> Version -> m Version
nextVersion isBreaking (Version [major, minor, revision])
  | isBreaking = pure $ Version [major, minor + 1, 0]
  | otherwise = pure $ Version [major, minor, revision + 1]
nextVersion _ v = fail $ "can't update version " <> show v

instance Parse Version where
  parse = parseText . pack
  parseText "latest" = pure LatestVersion
  parseText s =
    maybe
      (fail $ "invalid version: '" <> toString s <> "'!")
      (pure . Version)
      $ traverse (readMaybe . unpack)
      $ split (== '.') s

instance ToString Version where
  toString (Version ns) = intercalate "." $ map show ns
  toString LatestVersion = "latest"

instance Show Version where
  show = toString

instance ToText Version where
  toText = pack . toString

instance FromJSON Version where
  parseJSON (String s) = parseText s
  parseJSON (Number n) = parse (show n)
  parseJSON v = fail $ "version should be either true or string" <> show v

instance ToJSON Version where
  toJSON = String . pack . show

instance Ord Version where
  compare LatestVersion LatestVersion = EQ
  compare LatestVersion (Version _) = GT
  compare (Version _) LatestVersion = LT
  compare (Version v1) (Version v2) = compareSeries v1 v2
    where
      compareSeries [] _ = EQ
      compareSeries _ [] = EQ
      compareSeries (x : xs) (y : ys)
        | x == y = compareSeries xs ys
        | otherwise = compare x y

fetchVersionResponse :: (MonadIO m, MonadFail m) => String -> m (Either String (Map Text (NonEmpty Version)))
fetchVersionResponse name = hackage ["package", name, "preferred"]

lookupVersions :: (MonadFail m) => Either String (Map Text (NonEmpty Version)) -> m (NonEmpty Version)
lookupVersions (Right x) = maybe (fail "field normal-version not found") pure (lookup "normal-version" x)
lookupVersions (Left x) = fail x

fetchVersions :: (MonadFail m, MonadIO m) => String -> m (NonEmpty Version)
fetchVersions name = fetchVersionResponse name >>= lookupVersions