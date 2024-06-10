{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Internal.NameCollision
  ( NameCollision (..),
  )
where

import Data.Mergeable.Internal.Resolution (Indexed (..))
import Relude

class NameCollision e a where
  nameCollision :: a -> e

instance (NameCollision e a) => NameCollision e (Indexed k a) where
  nameCollision = nameCollision . indexedValue
