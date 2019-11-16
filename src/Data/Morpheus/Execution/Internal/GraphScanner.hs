module Data.Morpheus.Execution.Internal.GraphScanner
  ( LibUpdater
  , resolveUpdates
  )
where

import           Control.Monad                  ( foldM )
import           Data.Function                  ( (&) )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation )

type LibUpdater lib = lib -> Validation lib

-- Helper Functions
resolveUpdates :: lib -> [LibUpdater lib] -> Validation lib
resolveUpdates = foldM (&)
