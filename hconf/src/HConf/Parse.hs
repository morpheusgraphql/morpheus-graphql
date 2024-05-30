{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Parse
  ( Parse (..),
  )
where

import Relude

class Parse a where
  parse :: (ToString t, IsString t, Eq t, MonadFail m) => t -> m a
