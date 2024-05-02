{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Types
  ( Config (..),
    PkgGroup (..),
  )
where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (toJSON), Value (..), genericToJSON)
import Data.Aeson.Types (defaultOptions)
import Data.Text (intercalate, pack, split, unpack)
import Relude hiding (Undefined, intercalate)
import Prelude (read)

data Version = Version [Int]
  deriving
    ( Generic,
      Show
    )

parseVersion :: Text -> Version
parseVersion s = Version $ map (read . unpack) $ (split (== '.') s)

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
  [minV, maxV] -> pure $ VersionBounds (parseVersion minV) (Just (parseVersion maxV))
  [minV] -> pure $ VersionBounds (parseVersion minV) Nothing
  _ -> fail ("invalid version: " <> show s)

formatBounds :: Version -> Maybe Version -> Text
formatBounds mi Nothing = formatVersion mi
formatBounds mi (Just ma) = formatVersion mi <> "-" <> formatVersion ma

instance FromJSON VersionBounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseBounds s
  parseJSON (Number n) = pure $ VersionBounds (parseVersion $ show n) Nothing
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

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}
