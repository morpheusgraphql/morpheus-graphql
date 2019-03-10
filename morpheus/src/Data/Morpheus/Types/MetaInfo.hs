{-# LANGUAGE OverloadedStrings #-}
module Data.Morpheus.Types.MetaInfo
  ( MetaInfo(..)
  , initialMeta
  , Position(..)
  , LineBreaks
  )
where

import           Data.Text                      ( Text )

initialMeta :: MetaInfo
initialMeta = MetaInfo { position = 0, typeName = "", key = "" }

type LineBreaks = [Position];

type Position = Int;

data MetaInfo = MetaInfo {
  position :: Position,
  typeName ::  Text,
  key ::  Text
}
