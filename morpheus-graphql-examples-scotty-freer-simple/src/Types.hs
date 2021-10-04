module Types where

import           Data.Text                  (Text)

data Deity = Deity {
  name  :: Name,
  power :: Power
} deriving (Show)

type Name = Text
type Power = Maybe Text