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
    DirectivePrefix (..),
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
  visit :: a -> Visitor a TRUE -> GQLResult (Visitor a TRUE)

newtype DirectivePrefix = DirectivePrefix {name :: Text}

instance GQLDirective DirectivePrefix where
  type DIRECTIVE_LOCATION DirectivePrefix = '[ 'FIELD_DEFINITION, 'OBJECT, 'ENUM]
  visit a (VisitObject x) = pure (VisitObject x)
  visit a (VisitEnum x) = pure (VisitEnum x)
  visit a (VisitFieldDefinition x) = pure (VisitFieldDefinition x)
