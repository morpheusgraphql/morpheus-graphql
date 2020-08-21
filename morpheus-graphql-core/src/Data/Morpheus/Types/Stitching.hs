{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Stitching
  ( Stitching (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )

class Stitching a where
  stitch :: a -> a -> m a

instance Stitching (Schema VALID)
