{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Build GraphQL APIs with your favorite functional language!
module Config (setup) where

import Config.Types (Config (..))
import Relude hiding (ByteString, readFile, writeFile)

setup :: () -> Text
setup _ = ""