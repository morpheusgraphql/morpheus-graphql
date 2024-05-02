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

import Data.Aeson (FromJSON (..), Value (..))
import Data.Text (split, unpack)
import Relude hiding (Undefined)
import Prelude (read)

data Version = Version [Int]
  deriving
    ( Generic,
      Show
    )

parseInt :: Text -> Int
parseInt x = read (unpack x)

parseVersion :: Text -> Version
parseVersion s = Version $ map parseInt $ (split (== '.') s)

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

instance FromJSON VersionBounds where
  parseJSON (Bool True) = pure NoBounds
  parseJSON (String s) = parseBounds s
  parseJSON (Number n) = pure $ VersionBounds (parseVersion $ show n) Nothing
  parseJSON v = fail $ "version should be either true or string" <> (show v)

data PkgGroup = PkgGroup
  { dir :: Text,
    include :: [Text],
    prefix :: Maybe Text
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

type Deps = Map Text VersionBounds

data Build = Build
  { resolver :: Text,
    extra :: Maybe Deps,
    -- include :: [Text],
    skip :: Maybe [Text]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

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
