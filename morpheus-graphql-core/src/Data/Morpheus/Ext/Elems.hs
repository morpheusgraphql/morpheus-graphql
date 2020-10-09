{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Morpheus.Ext.Elems
  ( Elems (..),
    size,
  )
where

import qualified Data.HashMap.Lazy as HM
import Instances.TH.Lift ()
import Relude

class Elems a coll | coll -> a where
  elems :: coll -> [a]

instance Elems a (HashMap k a) where
  elems = HM.elems

instance Elems a [a] where
  elems = id

size :: Elems a coll => coll -> Int
size = length . elems
