{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
    Visitor (..),
    UserDirective (..),
  )
where

import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition,
    DirectiveLocation (..),
    FieldDefinition (..),
    INPUT_OBJECT,
    LEAF,
    OBJECT,
    OUT,
    TRUE,
    TypeDefinition (..),
    VALID,
  )
import Data.Text

type family Allow (x :: DirectiveLocation) (xs :: [DirectiveLocation]) :: Bool where
  Allow x '[] = 'False
  Allow x (x ': xs) = 'True
  Allow x (a ': xs) = Allow x xs

data Visitor a (t :: Bool) where
  -- Types
  VisitObject :: TypeDefinition OBJECT VALID -> Visitor a (Allow 'OBJECT (DIRECTIVE_LOCATION a))
  VisitInputObject :: TypeDefinition INPUT_OBJECT VALID -> Visitor a (Allow 'INPUT_OBJECT (DIRECTIVE_LOCATION a))
  VisitUnion :: TypeDefinition OUT VALID -> Visitor a (Allow 'UNION (DIRECTIVE_LOCATION a))
  VisitEnum :: TypeDefinition LEAF VALID -> Visitor a (Allow 'ENUM (DIRECTIVE_LOCATION a))
  VisitScalar :: TypeDefinition LEAF VALID -> Visitor a (Allow 'SCALAR (DIRECTIVE_LOCATION a))
  -- Fields
  VisitInputFieldDefinition :: (FieldDefinition INPUT_OBJECT VALID) -> Visitor a (Allow 'INPUT_FIELD_DEFINITION (DIRECTIVE_LOCATION a))
  VisitFieldDefinition :: (FieldDefinition OBJECT VALID) -> Visitor a (Allow 'FIELD_DEFINITION (DIRECTIVE_LOCATION a))
  VisitArgumentDefinition :: (ArgumentDefinition VALID) -> Visitor a (Allow 'ARGUMENT_DEFINITION (DIRECTIVE_LOCATION a))

class GQLDirective a where
  type DIRECTIVE_LOCATION a :: [DirectiveLocation]
  visit :: Visitor a TRUE -> GQLResult (Visitor a TRUE)

newtype UserDirective = UserDirective
  { name :: Text
  }

instance GQLDirective UserDirective where
  type DIRECTIVE_LOCATION UserDirective = '[ 'FIELD_DEFINITION, 'OBJECT, 'ENUM]
  visit (VisitObject x) = pure (VisitObject x)
  visit (VisitEnum x) = pure (VisitEnum x)
  visit (VisitFieldDefinition x) = pure (VisitFieldDefinition x)
