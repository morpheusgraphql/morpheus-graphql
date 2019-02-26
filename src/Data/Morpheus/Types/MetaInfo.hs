{-# LANGUAGE OverloadedStrings #-}
module Data.Morpheus.Types.MetaInfo (MetaInfo(..), initialMeta) where

import           Data.Text                      ( Text )

initialMeta :: MetaInfo
initialMeta = MetaInfo { className = "", cons = "", key = "" }

data MetaInfo = MetaInfo {
  -- TODO: position
  className ::  Text,
  cons::  Text,
  key ::  Text
}
