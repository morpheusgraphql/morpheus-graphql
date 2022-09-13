{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
  )
where

import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition,
    FieldDefinition (..),
    INPUT_OBJECT,
    LEAF,
    OBJECT,
    OUT,
    TypeDefinition (..),
    VALID,
  )
import Data.Text
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

type family Elem (x :: SchemaDirectiveLocation) (xs :: [SchemaDirectiveLocation]) :: Bool where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (a ': xs) = Elem x xs

type family If a l where
  If a 'True = a -> GQLResult a
  If a l = ()

type family DirRes a (x :: SchemaDirectiveLocation) v where
  DirRes a x v = If v (Elem x (DIRECTIVE_LOCATION a))

class GQLDirective a where
  type DIRECTIVE_LOCATION a :: [SchemaDirectiveLocation]

  -- types
  visitObject :: f a -> DirRes a 'OBJECT (TypeDefinition OBJECT VALID)
  visitInputObject :: f a -> DirRes a 'INPUT_OBJECT (TypeDefinition INPUT_OBJECT VALID)
  visitUnion :: f a -> DirRes a 'UNION (TypeDefinition OUT VALID)
  visitScalar :: f a -> DirRes a 'SCALAR (TypeDefinition OBJECT VALID)
  visitEnum :: f a -> DirRes a 'SCALAR (TypeDefinition LEAF VALID)

  -- elements
  visitInputFieldDefinition :: f a -> DirRes a 'INPUT_FIELD_DEFINITION (FieldDefinition INPUT_OBJECT VALID)
  visitFieldDefinition :: f a -> DirRes a 'FIELD_DEFINITION (FieldDefinition OBJECT VALID)
  visitArgumentDefinition :: f a -> DirRes a 'ARGUMENT_DEFINITION (ArgumentDefinition VALID)

data User = User
  { name :: Text
  }

instance GQLDirective User where
  type DIRECTIVE_LOCATION User = '[ 'FIELD_DEFINITION, 'OBJECT]
  visitFieldDefinition _ = pure
  visitObject _ = pure
  visitInputObject _ = ()
  visitUnion _ = ()
  visitScalar _ = ()
  visitEnum _ = ()
  visitInputFieldDefinition _ = ()
  visitArgumentDefinition _ = ()