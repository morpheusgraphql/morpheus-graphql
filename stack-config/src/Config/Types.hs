{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Types
  ( Config (..),
    PkgGroup (..),
    compareFields,
    getPackages,
    getBuild,
    Build (..),
  )
where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (toJSON), Value (..), genericToJSON)
import Data.Aeson.Types (defaultOptions)
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Text (intercalate, pack, split, toLower, unpack)
import Relude hiding (Undefined, intercalate)

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

data PkgGroup = PkgGroup
  { dir :: Text,
    names :: [Text],
    prefix :: Maybe Text
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON PkgGroup where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

type Deps = Map Text VersionBounds

data Build = Build
  { resolver :: Text,
    extra :: Maybe Deps,
    include :: Maybe [Text],
    exclude :: Maybe [Text]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

instance ToJSON Build where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data Config = Config
  { name :: Text,
    version :: Text,
    bounds :: Text,
    packages :: [PkgGroup],
    builds :: Map Text Build,
    dependencies :: Deps
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

getPackages :: Config -> [Text]
getPackages Config {..} = concatMap toPkg packages
  where
    toPkg PkgGroup {..} = map fullName names
      where
        fullName s 
        | length dir > 0 = dir <> "/" <> withPrefix s prefix
        | otherwise =  withPrefix s prefix

getBuild :: (MonadFail m) => Text -> Config -> m Build
getBuild key Config {builds} = maybe (fail "invalid version") pure (M.lookup key builds)

withPrefix :: Text -> Maybe Text -> Text
withPrefix "." (Just prefix) = prefix
withPrefix s (Just prefix) = prefix <> "-" <> s
withPrefix s _ = s

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

fields :: [Text]
fields = ["name", "version", "bounds", "resolver", "packages", "include", "exclude", "allow-newer", "save-hackage-creds", "extra-deps"]

compareFieldNames :: Text -> Text -> Ordering
compareFieldNames x y = case (findIndex (== x) fields, findIndex (== y) fields) of
  (Nothing, Nothing) -> case (parseVersion x, parseVersion y) of
    (Just v1, Just v2) -> compare v1 v2
    _ -> compare x y
  (Nothing, _) -> GT
  (_, Nothing) -> LT
  (i1, i2) -> compare i1 i2

compareFields :: Text -> Text -> Ordering
compareFields x y = compareFieldNames (toLower x) (toLower y)
