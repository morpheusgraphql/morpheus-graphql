module Data.Morpheus.Types.Internal.Base
  ( Key
  , Collection
  , Position
  , LineBreaks
  , EnhancedKey(..)
  , enhanceKeyWithNull
  ) where

import           Data.Text (Text)

type Position = Int

type LineBreaks = [Position]

type Key = Text

type Collection a = [(Key, a)]

-- Text value that includes position for debugging, where EnhancedKey "a" 1 === EnhancedKey "a" 3
data EnhancedKey = EnhancedKey
  { uid      :: Text
  , location :: Int
  }

instance Eq EnhancedKey where
  (EnhancedKey id1 _) == (EnhancedKey id2 _) = id1 == id2

enhanceKeyWithNull :: Key -> EnhancedKey
enhanceKeyWithNull text = EnhancedKey {uid = text, location = 0}
