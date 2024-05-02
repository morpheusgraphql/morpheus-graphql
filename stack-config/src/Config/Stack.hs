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

import Config.Types (Config, getPackages)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson.KeyMap (KeyMap, alterF)
import Relude hiding (Undefined, intercalate)

newtype Stack = Stack (KeyMap Value)
  deriving newtype
    ( ToJSON,
      FromJSON,
      Show
    )

updateStack :: (Monad m) => Config -> Stack -> m Stack
updateStack config (Stack stack) = do
  let packages = getPackages config
  Stack <$> (alterF (f packages) (fromString "packages") stack)
  where
    f packages _ = pure (Just (Array $ fromList $ map String packages))

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
