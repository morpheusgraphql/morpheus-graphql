{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Parse
  ( Parse (..),
  )
where

import Relude

class Parse a where
  parse :: (MonadFail m) => String -> m a
  parseText :: (MonadFail m) => Text -> m a
