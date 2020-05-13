module Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( GQLError (..),
  )
import Data.Morpheus.Types.Internal.Operation (KeyOf (..))

class NameCollision a where
  nameCollision :: KEY a -> a -> GQLError
