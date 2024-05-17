{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Version
  ( Version (..),
    parseVersion,
    VersionBounds (..),
    Deps,
    parseBounds,
    parseDep,
    printBounds,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Char (isSeparator)
import Data.Text
  ( break,
    breakOn,
    drop,
    null,
    pack,
    split,
    strip,
    unpack,
  )
import GHC.Show (Show (show))
import Relude hiding
  ( Undefined,
    break,
    drop,
    isPrefixOf,
    length,
    null,
    show,
  )

data Version
  = Version [Int]
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

instance ToString Version where
  toString (Version ns) = intercalate "." $ map show ns
  toString LatestVersion = "latest"

instance Show Version where
  show = toString

instance ToText Version where
  toText = pack . toString

instance FromJSON Version where
  parseJSON (String s) = parseVersion s
  parseJSON (Number n) = parseVersion $ pack $ show n
  parseJSON v = fail $ "version should be either true or string" <> (show v)

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

parseVersion :: (MonadFail m) => Text -> m Version
parseVersion "latest" = pure LatestVersion
parseVersion s =
  maybe
    (fail "invalid version")
    (pure . Version)
    $ traverse (readMaybe . unpack)
    $ (split (== '.') s)

data VersionBounds
  = VersionBounds Version (Maybe Version)
  | NoBounds
  deriving
    ( Generic,
      Show,
      Eq,
      Ord
    )

trim :: (Text, Text) -> (Text, Text)
trim = bimap strip strip

breakOnSPace :: Text -> (Text, Text)
breakOnSPace = trim . break isSeparator

parseVersionTuple :: (MonadFail m) => (Text, Text) -> m VersionBounds
parseVersionTuple (mi, ma) = do
  ((,) <$> parseMin (breakOnSPace mi) <*> parseMax (breakOnSPace ma))
    >>= uncurry parseBoundsFrom

breakAtAnd :: Text -> (Text, Text)
breakAtAnd = trim . second (drop 2) . (breakOn "&&")

parseDep :: (MonadFail m) => Text -> m (Text, VersionBounds)
parseDep = (\(name, bounds) -> (name,) <$> parseVersionBounds bounds) . breakOnSPace

parseVersionBounds :: (MonadFail m) => Text -> m VersionBounds
parseVersionBounds bounds
  | null bounds = pure NoBounds
  | otherwise = parseVersionTuple (breakAtAnd bounds)

parseMax :: (MonadFail m) => (Text, Text) -> m (Maybe Text)
parseMax (o, value) | isUpperConstraint o = pure (Just value)
parseMax _ = pure Nothing

parseMin :: (MonadFail m) => (Text, Text) -> m Text
parseMin (o, value) | isLowerConstraint o = pure value
parseMin (o, v) = fail ("invalid" <> show (o, v))

isLowerConstraint :: Text -> Bool
isLowerConstraint = (`elem` [">", ">="])

isUpperConstraint :: Text -> Bool
isUpperConstraint = (`elem` ["<", "<="])

parseBoundsFrom :: (MonadFail m) => Text -> Maybe Text -> m VersionBounds
parseBoundsFrom minV maxV = VersionBounds <$> (parseVersion minV) <*> (traverse parseVersion maxV)

printBounds :: VersionBounds -> [Text]
printBounds NoBounds = []
printBounds (VersionBounds mi ma) = [">=", toText mi] <> maybe [] (\m -> ["&&", "<", toText m]) ma

parseBounds :: (MonadFail m) => Text -> m VersionBounds
parseBounds s = case (split (== '-') s) of
  [minV, maxV] -> parseBoundsFrom minV (Just maxV)
  [minV] -> parseBoundsFrom minV Nothing
  _ -> fail ("invalid version: " <> show s)

formatBounds :: Version -> Maybe Version -> Text
formatBounds mi Nothing = pack $ show mi
formatBounds mi (Just ma) = pack $ show mi <> " && " <> show ma

instance FromJSON VersionBounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseBounds s
  parseJSON (Number n) = flip VersionBounds Nothing <$> (parseVersion $ pack $ show n)
  parseJSON v = fail $ "version should be either true or string" <> (show v)

instance ToJSON VersionBounds where
  toJSON NoBounds = (Bool True)
  toJSON (VersionBounds mi ma) = String $ formatBounds mi ma

type Deps = Map Text VersionBounds
