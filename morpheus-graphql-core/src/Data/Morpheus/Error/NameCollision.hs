module Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( GQLError (..),
    Name,
  )

class NameCollision a where
  nameCollision :: Name -> a -> GQLError
