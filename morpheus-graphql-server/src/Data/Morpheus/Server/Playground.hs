{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Playground
  ( httpPlayground,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString, fromStrict)
import Data.FileEmbed (embedFile, makeRelativeToProject)

httpPlayground :: ByteString
httpPlayground = fromStrict $(embedFile =<< makeRelativeToProject "src/Data/Morpheus/Server/Playground/index.html")
