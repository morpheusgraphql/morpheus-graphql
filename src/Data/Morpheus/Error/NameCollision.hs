module Data.Morpheus.Error.NameCollision
  (NameCollision(..)
  )
where

import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Name 
                                                , GQLError(..)
                                                )

class NameCollision a where 
  nameCollision :: Name -> a -> GQLError

class UndefinedValue c where
  undefinedValue :: c -> GQLError
