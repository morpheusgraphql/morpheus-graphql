{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Config
  ( Config (..),
  )
where

import Prelude
  ( Bool,
    Show,
  )

newtype Config = Config
  { debug :: Bool
  }
  deriving (Show)
