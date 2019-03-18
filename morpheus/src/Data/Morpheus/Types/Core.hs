module Data.Morpheus.Types.Core
  ( Key
  , Collection
  ) where

import           Data.Text (Text)

type Key = Text

type Collection a = [(Key, a)]
