{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.Empty
  ( Empty (..),
  )
where

import qualified Data.HashMap.Lazy as HM

class Empty coll where
  empty :: coll

instance Empty [a] where
  empty = []

instance Empty (HM.HashMap k v) where
  empty = HM.empty
