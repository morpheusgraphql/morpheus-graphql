{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
    Visitor (..),
    getLocations,
    ToLocations (..),
    VisitorFunction (..),
  )
where

import Data.Morpheus.App
  ( VisitorFieldDefinition (..),
    VisitorTypeDefinition (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Types.TypeName (getTypename)
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
    FieldName,
    TRUE,
  )
import Relude

type family Allow (x :: DirectiveLocation) (xs :: [DirectiveLocation]) :: Bool where
  Allow x '[] = 'False
  Allow x (x ': xs) = 'True
  Allow x (a ': xs) = Allow x xs

type VisitorOption (k :: DirectiveLocation) (a :: Type) = Visitor a (Allow k (ALLOWED_DIRECTIVE_LOCATIONS a))

class ToLocation (l :: DirectiveLocation) where
  toLocation :: f l -> DirectiveLocation

instance ToLocation 'OBJECT where
  toLocation = const OBJECT

instance ToLocation 'ENUM where
  toLocation = const ENUM

instance ToLocation 'INPUT_OBJECT where
  toLocation = const INPUT_OBJECT

instance ToLocation 'UNION where
  toLocation = const UNION

instance ToLocation 'INPUT_FIELD_DEFINITION where
  toLocation = const INPUT_FIELD_DEFINITION

instance ToLocation 'ARGUMENT_DEFINITION where
  toLocation = const ARGUMENT_DEFINITION

instance ToLocation 'FIELD_DEFINITION where
  toLocation = const FIELD_DEFINITION

instance ToLocation 'SCALAR where
  toLocation = const SCALAR

instance ToLocation 'INTERFACE where
  toLocation = const INTERFACE

instance ToLocation 'ENUM_VALUE where
  toLocation = const ENUM_VALUE

class ToLocations (k :: [DirectiveLocation]) where
  toLocations :: f k -> [DirectiveLocation]

instance (ToLocation x, ToLocations xs) => ToLocations (x : xs) where
  toLocations _ = toLocation (Proxy @x) : toLocations (Proxy @xs)

instance ToLocations '[] where
  toLocations _ = []

getLocations :: forall f a. ToLocations (ALLOWED_DIRECTIVE_LOCATIONS a) => f a -> [DirectiveLocation]
getLocations _ = toLocations (Proxy :: Proxy (ALLOWED_DIRECTIVE_LOCATIONS a))

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

class Typeable a => GQLDirective a where
  type ALLOWED_DIRECTIVE_LOCATIONS a :: [DirectiveLocation]

  visit :: a -> Visitor a TRUE -> GQLResult (Visitor a TRUE)

  __directiveName :: f a -> FieldName
  __directiveName = coerce . getTypename

data VisitorFunction where
  VisitorFunction :: VisitorFunction

-- TODO: fix me
-- Visitor a TRUE -> GQLResult (Visitor a TRUE) -> VisitorFunction
