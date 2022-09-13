{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
    Visitor (..),
    DirectivePrefix (..),
    VisitorTypeDefinition (..),
  )
where

import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST
  ( Description,
    DirectiveLocation (..),
    FieldName,
    TRUE,
    TypeName,
    TypeRef (..),
  )

type family Allow (x :: DirectiveLocation) (xs :: [DirectiveLocation]) :: Bool where
  Allow x '[] = 'False
  Allow x (x ': xs) = 'True
  Allow x (a ': xs) = Allow x xs

data VisitorTypeDefinition = VisitorTypeDefinition
  { visitorTypeName :: TypeName,
    visitorTypeDescription :: Maybe Description
  }
  deriving (Show, Eq)

data VisitorFieldDefinition = FieldDefinition
  { visitorFieldDescription :: Maybe Description,
    visitorFieldName :: FieldName,
    visitorFieldType :: TypeRef
  }
  deriving (Show, Eq)

-- data VisitorObject = VisitorObject
--   { visitorObjectName :: TypeName,
--     visitorObjectImplements :: [TypeName],
--     visitorObjectFields :: FieldsDefinition OUT VALID
--   }

-- data VisitorInputObject = VisitorInputObject
--   { visitorInputObjectName :: TypeName,
--     visitorInputObjectFields :: FieldsDefinition IN VALID
--   }

-- newtype VisitorUnion = VisitorUnion
--   { visitorUnionMembers :: [TypeName]
--   }

-- newtype VisitorUnion = VisitorUnion
--   { visitorEnumValues :: [DataEnumValue s]
--   }

data Visitor a (t :: Bool) where
  -- Types
  VisitObject :: VisitorTypeDefinition -> Visitor a (Allow 'OBJECT (DIRECTIVE_LOCATION a))
  VisitInputObject :: VisitorTypeDefinition -> Visitor a (Allow 'INPUT_OBJECT (DIRECTIVE_LOCATION a))
  VisitUnion :: VisitorTypeDefinition -> Visitor a (Allow 'UNION (DIRECTIVE_LOCATION a))
  VisitEnum :: VisitorTypeDefinition -> Visitor a (Allow 'ENUM (DIRECTIVE_LOCATION a))
  VisitScalar :: VisitorTypeDefinition -> Visitor a (Allow 'SCALAR (DIRECTIVE_LOCATION a))
  -- Fields
  -- VisitArgumentDefinition :: (ArgumentDefinition VALID) -> Visitor a (Allow 'ARGUMENT_DEFINITION (DIRECTIVE_LOCATION a))
  VisitInputFieldDefinition :: VisitorFieldDefinition -> Visitor a (Allow 'INPUT_FIELD_DEFINITION (DIRECTIVE_LOCATION a))
  VisitFieldDefinition :: VisitorFieldDefinition -> Visitor a (Allow 'FIELD_DEFINITION (DIRECTIVE_LOCATION a))

class GQLDirective a where
  type DIRECTIVE_LOCATION a :: [DirectiveLocation]
  visit :: a -> Visitor a TRUE -> GQLResult (Visitor a TRUE)

data DirectivePrefix = DirectivePrefix
  { prefix :: TypeName,
    drop :: Bool
  }

prefixName :: TypeName -> VisitorTypeDefinition -> VisitorTypeDefinition
prefixName prefix t = t {visitorTypeName = prefix <> visitorTypeName t}

instance GQLDirective DirectivePrefix where
  type DIRECTIVE_LOCATION DirectivePrefix = '[ 'OBJECT, 'ENUM, 'INPUT_OBJECT]

  visit DirectivePrefix {prefix} (VisitEnum t) = pure $ VisitEnum $ prefixName prefix t
  visit DirectivePrefix {prefix} (VisitInputObject t) = pure $ VisitInputObject $ prefixName prefix t
  visit DirectivePrefix {prefix} (VisitObject t) = pure $ VisitObject $ prefixName prefix t
