{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Types
  ( Config (..),
     Libs(..)
  )
where

import Control.Monad.Except (MonadError (..))
import Relude hiding (Undefined)


data Libs = Libs {
  dir:: Text,
  include :: Text
} deriving (Show)


data Config = Config
  { name:: Text,
    version: Text,
    bounds: Text,
    packages:: []
  }
  deriving (Show)
