{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
  )
where

import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST
  ( FieldDefinition (..),
    INPUT_OBJECT,
    OBJECT,
    OUT,
    TypeDefinition (..),
    VALID,
  )
import Language.Haskell.TH.Syntax (Lift)

data SchemaDirectiveLocation
  = SCALAR
  | OBJECT
  | FIELD_DEFINITION
  | INPUT_FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  deriving (Show, Eq, Lift)

type family DirRes a l where
  DirRes a 'True = a -> GQLResult a
  DirRes a l = ()

class GQLDirective a where
  type DIRECTIVE_LOCATION a (l :: SchemaDirectiveLocation) :: Bool

  visitObject :: f a -> DirRes (TypeDefinition OBJECT VALID) (DIRECTIVE_LOCATION a 'OBJECT)
  visitInputObject :: f a -> DirRes (TypeDefinition INPUT_OBJECT VALID) (DIRECTIVE_LOCATION a 'INPUT_OBJECT)
  visitUnion :: f a -> DirRes (TypeDefinition OUT VALID) (DIRECTIVE_LOCATION a 'UNION)

  visitInputFieldDefinition :: f a -> DirRes (FieldDefinition INPUT_OBJECT VALID) (DIRECTIVE_LOCATION a 'INPUT_FIELD_DEFINITION)
  visitFieldDefinition :: f a -> DirRes (FieldDefinition OBJECT VALID) (DIRECTIVE_LOCATION a 'FIELD_DEFINITION)

-- visitArgumentDefinition :: GraphQLArgument

-- visitScalar :: (scalar : GraphQLScalarType)

-- visitSchema(schema: GraphQLSchema)
-- visitInterface(i: GraphQLInterfaceType)
-- visitEnum :: GraphQLEnumType
-- visitEnumValue :: (value : GraphQLEnumValue)

data User = User {}

instance GQLDirective User where
  type DIRECTIVE_LOCATION User 'FIELD_DEFINITION = 'True
  type DIRECTIVE_LOCATION User 'OBJECT = 'True
  visitFieldDefinition _ = pure
  visitObject _ = pure