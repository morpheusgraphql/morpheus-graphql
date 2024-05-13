{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Version
  ( Version (..),
    parseVersion,
    VersionBounds (..),
    Deps,
    parseBounds,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (toJSON),
    Value (..),
  )
import Data.Text
  ( pack,
    split,
    unpack,
  )
import GHC.Show (Show (show))
import Relude hiding
  ( Undefined,
    show,
  )

data Version
  = Version [Int]
  | LatestVersion
  deriving
    ( Generic,
      Eq
    )

instance Show Version where
  show (Version ns) = intercalate "." $ map show ns
  show LatestVersion = "latest"

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

parseMaybeVersion :: Text -> Maybe Version
parseMaybeVersion "latest" = pure LatestVersion
parseMaybeVersion s = Version <$> (traverse (readMaybe . unpack) $ (split (== '.') s))

parseVersion :: (MonadFail m) => Text -> m Version
parseVersion s = maybe (fail "invalid version") pure (parseMaybeVersion s)

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
formatBounds mi Nothing = pack $ show mi
formatBounds mi (Just ma) = pack $ show mi <> "-" <> show ma

instance FromJSON VersionBounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseBounds s
  parseJSON (Number n) = flip VersionBounds Nothing <$> (parseVersion $ pack $ show n)
  parseJSON v = fail $ "version should be either true or string" <> (show v)

instance ToJSON VersionBounds where
  toJSON NoBounds = (Bool True)
  toJSON (VersionBounds mi ma) = String $ formatBounds mi ma

type Deps = Map Text VersionBounds
