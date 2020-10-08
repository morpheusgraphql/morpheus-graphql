module Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
  )
where

import Relude

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

data Config = Config
  { debug :: Bool,
    validationMode :: VALIDATION_MODE
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { debug = False,
      validationMode = FULL_VALIDATION
    }

debugConfig :: Config
debugConfig =
  Config
    { debug = True,
      validationMode = FULL_VALIDATION
    }
