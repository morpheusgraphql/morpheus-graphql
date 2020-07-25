{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
  )
where

import Prelude
  ( Bool,
    Eq,
    Show,
  )

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

data Config = Config
  { debug :: Bool,
    validationMode :: VALIDATION_MODE
  }
  deriving (Show)
