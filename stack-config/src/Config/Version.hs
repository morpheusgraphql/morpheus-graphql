{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Version
  ( Version (..),
    parseVersion,
    VersionBounds,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Text
  ( intercalate,
    pack,
    split,
    unpack,
  )
import Relude hiding
  ( Undefined,
    intercalate,
  )

data Version = Version [Int]
  deriving
    ( Generic,
      Show,
      Eq
    )

instance Ord Version where
  compare (Version v1) (Version v2) = compareSeries v1 v2
    where
      compareSeries [] _ = EQ
      compareSeries _ [] = EQ
      compareSeries (x : xs) (y : ys)
        | x == y = compareSeries xs ys
        | otherwise = compare x y

parseMaybeVersion :: Text -> Maybe Version
parseMaybeVersion s = Version <$> (traverse (readMaybe . unpack) $ (split (== '.') s))

parseVersion :: (MonadFail m) => Text -> m Version
parseVersion = maybe (fail "invalid version") pure . parseMaybeVersion

formatVersion :: Version -> Text
formatVersion (Version ns) = intercalate "." $ map (pack . show) ns

data VersionBounds
  = VersionBounds Version (Maybe Version)
  | NoBounds
  deriving
    ( Generic,
      Show
    )

parseBounds :: (MonadFail m) => Text -> m VersionBounds
parseBounds s = case (split (== '-') s) of
  [minV, maxV] -> VersionBounds <$> (parseVersion minV) <*> (fmap Just (parseVersion maxV))
  [minV] -> flip VersionBounds Nothing <$> (parseVersion minV)
  _ -> fail ("invalid version: " <> show s)

formatBounds :: Version -> Maybe Version -> Text
formatBounds mi Nothing = formatVersion mi
formatBounds mi (Just ma) = formatVersion mi <> "-" <> formatVersion ma

instance FromJSON VersionBounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseBounds s
  parseJSON (Number n) = flip VersionBounds Nothing <$> (parseVersion $ show n)
  parseJSON v = fail $ "version should be either true or string" <> (show v)

instance ToJSON VersionBounds where
  toJSON NoBounds = (Bool True)
  toJSON (VersionBounds mi ma) = String $ formatBounds mi ma
