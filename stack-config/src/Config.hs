{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( Config (..),
    PkgGroup (..),
    parseYaml,
    serializeYaml,
    Stack (..),
  )
where

import Config.Stack
import Config.Types
import Data.Aeson (FromJSON, ToJSON)
import Data.Yaml (decodeThrow)
import Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare, setConfDropNull)
import Relude

parseYaml :: (FromJSON a) => ByteString -> IO a
parseYaml = decodeThrow

serializeYaml :: (ToJSON a) => a -> ByteString
serializeYaml = encodePretty (setConfDropNull True $ setConfCompare compareFields defConfig)

-- export const setup = async (version: string) => {
--   const config = await Config.load();

--   log("generating:\n");

--   Stack.write(await getStack(version));

--   ok(`${defs.STACK} (ghc ${version})`);

--   hie([...config.packages(), "morpheus-graphql-benchmarks"]);

--   ok(defs.HIE);

--   checkPackages();
-- };