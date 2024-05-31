{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Core.Version
  ( nextVersion,
    dropPatch,
    fetchVersions,
    Version,
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

newtype Version = Version [Int]
  deriving
    ( Generic,
      Eq
    )

nextVersion :: (MonadFail m) => Bool -> Version -> m Version
nextVersion isBreaking (Version [major, minor, revision])
  | isBreaking = pure $ Version [major, minor + 1, 0]
  | otherwise = pure $ Version [major, minor, revision + 1]
nextVersion _ v = fail $ "can't update version " <> show v

dropPatch :: Version -> Version
dropPatch (Version xs) = Version (take 2 xs <> [0])

compareSeries :: (Ord a) => [a] -> [a] -> Ordering
compareSeries [] _ = EQ
compareSeries _ [] = EQ
compareSeries (x : xs) (y : ys)
  | x == y = compareSeries xs ys
  | otherwise = compare x y

instance Parse Version where
  parse = parseText . pack
  parseText s =
    maybe
      (fail $ "invalid version: '" <> toString s <> "'!")
      (pure . Version)
      $ traverse (readMaybe . unpack)
      $ split (== '.') s

instance ToString Version where
  toString (Version ns) = intercalate "." $ map show ns

instance Ord Version where
  compare (Version v1) (Version v2) = compareSeries v1 v2

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

fetchVersionResponse :: (MonadIO m, MonadFail m) => String -> m (Either String (Map Text (NonEmpty Version)))
fetchVersionResponse name = hackage ["package", name, "preferred"]

lookupVersions :: (MonadFail m) => Either String (Map Text (NonEmpty Version)) -> m (NonEmpty Version)
lookupVersions (Right x) = maybe (fail "field normal-version not found") pure (lookup "normal-version" x)
lookupVersions (Left x) = fail x

fetchVersions :: (MonadFail m, MonadIO m) => String -> m (NonEmpty Version)
fetchVersions name = fetchVersionResponse name >>= lookupVersions