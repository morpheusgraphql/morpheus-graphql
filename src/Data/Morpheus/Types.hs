module Data.Morpheus.Types
  ( ScalarValue(..)
  , (::->)(Resolver)
  , GQLRoot(..)
  ) where

import           Data.Morpheus.Types.Describer ((::->) (Resolver))
import           Data.Morpheus.Types.JSType    (ScalarValue (..))
import           Data.Morpheus.Types.Types     (GQLRoot (..))
