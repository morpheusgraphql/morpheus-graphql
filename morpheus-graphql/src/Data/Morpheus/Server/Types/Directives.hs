{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Server.Types.Directives
  ( DIRECTIVE_LOCATION,
    GQLDirective (..),
  )
where

import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST (DirectiveLocation (..), FieldDefinition (FieldDefinition), TypeDefinition (TypeDefinition), VALID)

type family DIRECTIVE_LOCATION a (l :: DirectiveLocation) :: Bool

type family DirRes a (l :: Bool) where
  DirRes a 'True = a -> GQLResult a
  DirRes a 'False = ()

class GQLDirective a where
  mapObject :: a -> DirRes (TypeDefinition k b) (DIRECTIVE_LOCATION a 'OBJECT)
  mapField :: a -> DirRes (FieldDefinition k b) (DIRECTIVE_LOCATION a 'FIELD)

-- example

data User = User {}

type instance DIRECTIVE_LOCATION User 'FIELD = 'True

type instance DIRECTIVE_LOCATION User 'OBJECT = 'False