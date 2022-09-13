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
    DirectiveUsage (..),
  )
where

import Data.Kind (Type)
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

type VisitorOption (k :: DirectiveLocation) (a :: Type) = Visitor a (Allow k (ALLOWED_DIRECTIVE_LOCATIONS a))

data Visitor a (t :: Bool) where
  -- Types
  VisitObject :: VisitorTypeDefinition -> VisitorOption 'OBJECT a
  VisitInputObject :: VisitorTypeDefinition -> VisitorOption 'INPUT_OBJECT a
  VisitUnion :: VisitorTypeDefinition -> VisitorOption 'UNION a
  VisitEnum :: VisitorTypeDefinition -> VisitorOption 'ENUM a
  VisitScalar :: VisitorTypeDefinition -> VisitorOption 'SCALAR a
  -- Fields
  -- VisitArgumentDefinition :: (ArgumentDefinition VALID) -> Visitor a (Allow 'ARGUMENT_DEFINITION (DIRECTIVE_LOCATION a))
  VisitInputFieldDefinition :: VisitorFieldDefinition -> VisitorOption 'INPUT_FIELD_DEFINITION a
  VisitFieldDefinition :: VisitorFieldDefinition -> VisitorOption 'FIELD_DEFINITION a

data DirectiveUsage where
  DirectiveUsage :: GQLDirective a => a -> DirectiveUsage

class GQLDirective a where
  type ALLOWED_DIRECTIVE_LOCATIONS a :: [DirectiveLocation]

  visit :: a -> Visitor a TRUE -> GQLResult (Visitor a TRUE)

data DirectivePrefix = DirectivePrefix
  { prefix :: TypeName,
    drop :: Bool
  }

prefixName :: TypeName -> VisitorTypeDefinition -> VisitorTypeDefinition
prefixName prefix t = t {visitorTypeName = prefix <> visitorTypeName t}

instance GQLDirective DirectivePrefix where
  type ALLOWED_DIRECTIVE_LOCATIONS DirectivePrefix = '[ 'OBJECT, 'ENUM, 'INPUT_OBJECT]

  visit DirectivePrefix {prefix} (VisitEnum t) = pure $ VisitEnum $ prefixName prefix t
  visit DirectivePrefix {prefix} (VisitInputObject t) = pure $ VisitInputObject $ prefixName prefix t
  visit DirectivePrefix {prefix} (VisitObject t) = pure $ VisitObject $ prefixName prefix t
