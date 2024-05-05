{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Types
  ( Config (..),
    PkgGroup (..),
    compareFields,
    getPackages,
    getBuild,
    Build (..),
    getBuilds,
  )
where

import Config.Version (Deps, Version, parseVersion)
import Data.Aeson (FromJSON (..), Options (..), ToJSON (toJSON), genericToJSON)
import Data.Aeson.Types (defaultOptions)
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Text (toLower)
import Relude hiding (Undefined, intercalate)

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

data Build = Build
  { resolver :: Text,
    extra :: Maybe (Map Text Version),
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
          | dir /= "./" = dir <> "/" <> withPrefix s prefix
          | otherwise = withPrefix s prefix

getBuild :: (MonadFail m) => Version -> Config -> m Build
getBuild key Config {builds} = maybe (fail "invalid version") pure (M.lookup (show key) builds)

getBuilds :: (MonadFail m) => Config -> m [(Version, Build)]
getBuilds Config {builds} = traverse (\(k, v) -> (,v) <$> parseVersion k) (M.toList builds)

withPrefix :: Text -> Maybe Text -> Text
withPrefix "." (Just prefix) = prefix
withPrefix s (Just prefix) = prefix <> "-" <> s
withPrefix s _ = s

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

fields :: [Text]
fields =
  [ "name",
    "version",
    "github",
    "license",
    "author",
    "category",
    "synopsis",
    "copyright",
    "description",
    "dependencies",
    "data-files",
    "bounds",
    "resolver",
    "packages",
    "include",
    "exclude",
    "allow-newer",
    "save-hackage-creds",
    "extra-deps",
    "stackyaml",
    "components",
    "path",
    "component"
  ]

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
