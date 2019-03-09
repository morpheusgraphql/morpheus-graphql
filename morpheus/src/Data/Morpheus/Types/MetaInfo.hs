{-# LANGUAGE OverloadedStrings #-}
module Data.Morpheus.Types.MetaInfo
  ( MetaInfo(..)
  , initialMeta
  , Position(..)
  , LineMarks
  )
where

import           Data.Text                      ( Text )

initialMeta :: MetaInfo
initialMeta = MetaInfo { position = Position 0, typeName = "", key = "" }

type LineMarks = [Int];

newtype Position = Position { getPos :: Int } deriving (Show)

data MetaInfo = MetaInfo {
  position :: Position,
  typeName ::  Text,
  key ::  Text
}
