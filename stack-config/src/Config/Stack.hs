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

import Config.Types (Build (..), Config, getBuild, getPackages)
import Control.Monad (foldM)
import Data.Aeson (FromJSON (..), Key, ToJSON (..), Value (..))
import Data.Aeson.KeyMap (KeyMap, alterF)
import Relude hiding (Undefined, intercalate)

newtype Stack = Stack (KeyMap Value)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Show
    )

set :: (Applicative f) => KeyMap v -> (Key, v) -> f (KeyMap v)
set v (k, p) = alterF (\_ -> pure (Just p)) k v

fields :: (Monad f) => [(Key, v)] -> KeyMap v -> f (KeyMap v)
fields fs stack = foldM set stack fs

updateStack :: (MonadFail m) => Config -> Stack -> m Stack
updateStack config (Stack stack) = do
  let packages = Array $ fromList $ map String $ getPackages config
  Build {..} <- getBuild "9.0.2" config
  Stack
    <$> fields
      [ ("packages", packages),
        ("resolver", String resolver)
      ]
      stack

-- const getStack = async (version: string) => {
--   const config = await Config.load();
--   const { include = [], resolver, exclude = [] } = config.plan(version);
--   const extra = config
--     .plans()
--     .filter((v) => Version.compare(v, version) >= 0)
--     .flatMap((v) => Object.entries(config.plan(v).extra ?? {}))
--     .map(([key, val]) => `${key}-${val}`)
--     .sort();
--   const packages = difference([...config.packages(), ...include], exclude);

--   return {
--     ...(version === "latest" ? { "allow-newer": true } : {}),
--     resolver,
--     "save-hackage-creds": false,
--     packages,
--     "extra-deps": extra,
--   };
-- };

-- const ok = (msg: string) => log(` - ${msg}\n`, "success");
