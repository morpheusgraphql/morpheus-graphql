{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App
  ( Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
    App (..),
    AppData (..),
    runApp,
    withDebugger,
    mkApp,
    runAppStream,
  )
where

import Data.Morpheus.Core
  ( Config (..),
    VALIDATION_MODE (..),
    debugConfig,
    defaultConfig,
  )
import Data.Morpheus.Types.App
  ( App (..),
    AppData (..),
    mkApp,
    runApp,
    runAppStream,
    withDebugger,
  )
