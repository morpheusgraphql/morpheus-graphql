module Data.Morpheus.Generics.InputType (InputType(..)) where

import           Data.Morpheus.Types.Types     (JSType(..))
import           Data.Text                     (Text)

class InputType a where
    decode :: JSType -> a

instance InputType Text where
    decode  (JSString x) = x

instance InputType Bool where
    decode  (JSBool x) = x