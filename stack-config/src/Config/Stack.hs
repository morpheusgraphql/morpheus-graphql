{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Stack
  ( Stack,
    updateStack,
  )
where

import Config.Types (Build (..), Config, getBuild, getBuilds, getPackages)
import Config.Version (Version (..), parseVersion)
import Control.Monad (foldM)
import Data.Aeson (FromJSON (..), Key, ToJSON (..), Value (..))
import Data.Aeson.KeyMap (KeyMap, alterF)
import Data.List ((\\))
import qualified Data.Map as M
import Relude hiding (Undefined, intercalate)

newtype Stack = Stack (KeyMap Value)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Show
    )

set :: (Applicative f) => KeyMap v -> (Key, v) -> f (KeyMap v)
set v (k, p) = alterF (\_ -> pure (Just p)) k v

setFields :: (Monad f) => [(Key, v)] -> KeyMap v -> f (KeyMap v)
setFields fs stack = foldM set stack fs

updateStack :: (MonadFail m) => Text -> Config -> Stack -> m Stack
updateStack v config (Stack stack) = do
  version <- parseVersion v
  Build {..} <- getBuild version config
  extraDeps <- getExtraDeps version <$> getBuilds config
  Stack
    <$> setFields
      [ ("packages", Array $ fromList $ map String $ (getPackages config <> fromMaybe [] include) \\ fromMaybe [] exclude),
        ("resolver", String resolver),
        ("allow-newer", Bool (allowNewer version)),
        ("save-hackage-creds", Bool False),
        ("extra-deps", Array $ fromList $ map String $ extraDeps)
      ]
      stack

allowNewer :: Version -> Bool
allowNewer LatestVersion = True
allowNewer _ = False

getExtraDeps :: Version -> [(Version, Build)] -> [Text]
getExtraDeps v xs = concatMap f $ filter includeVersion xs
  where
    includeVersion (k, _) = v > k
    f (_, b) = map printExtra $ maybe [] M.toList (extra b)
    printExtra (k, ver) = k <> show ver

--     .plans()
--     .filter((v) => Version.compare(v, version) >= 0)
--     .flatMap((v) => Object.entries(config.plan(v).extra ?? {}))
--     .map(([key, val]) => `${key}-${val}`)
--     .sort();