{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.Elems
  ( Elems (..),
    size,
  )
where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Instances.TH.Lift ()
import Prelude
  ( (.),
    Int,
    id,
    length,
  )

class Elems a coll | coll -> a where
  elems :: coll -> [a]

instance Elems a (HashMap k a) where
  elems = HM.elems

instance Elems a [a] where
  elems = id

size :: Elems a coll => coll -> Int
size = length . elems
