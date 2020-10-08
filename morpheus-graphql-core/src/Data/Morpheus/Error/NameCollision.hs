module Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
where

import Data.Morpheus.Ext.Map (Indexed (..))
import Data.Morpheus.Types.Internal.AST.Base
  ( ValidationError,
  )
import Prelude
  ( (.),
  )

class NameCollision a where
  nameCollision :: a -> ValidationError

instance NameCollision a => NameCollision (Indexed k a) where
  nameCollision = nameCollision . indexedValue
