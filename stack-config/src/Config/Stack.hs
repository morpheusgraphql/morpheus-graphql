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
import Control.Monad (foldM)
import Data.Aeson (FromJSON (..), Key, ToJSON (..), Value (..))
import Data.Aeson.KeyMap (KeyMap, alterF)
import Data.List ((\\))
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
updateStack version config (Stack stack) = do
  Build {..} <- getBuild version config
  let extra = getBuilds config
  Stack
    <$> setFields
      [ ("packages", Array $ fromList $ map String $ (getPackages config <> fromMaybe [] include) \\ fromMaybe [] exclude),
        ("resolver", String resolver),
        ("allow-newer", Bool $ allowNewer version),
        ("save-hackage-creds", Bool False),
        ("extra-deps", Null)
      ]
      stack

allowNewer :: (Eq a, IsString a) => a -> Bool
allowNewer "latest" = True
allowNewer _ = False

--   const extra = config
--     .plans()
--     .filter((v) => Version.compare(v, version) >= 0)
--     .flatMap((v) => Object.entries(config.plan(v).extra ?? {}))
--     .map(([key, val]) => `${key}-${val}`)
--     .sort();