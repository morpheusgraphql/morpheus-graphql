module Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
where

import Data.Morpheus.Internal.Utils (KeyOf (..))
import Data.Morpheus.Types.Internal.AST.Base
  ( GQLError (..),
  )

class NameCollision a where
  nameCollision :: KEY a -> a -> GQLError
