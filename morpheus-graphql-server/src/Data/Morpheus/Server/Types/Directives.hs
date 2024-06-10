{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
    ToLocations (..),
    getLocations,
    GDirectiveUsage (..),
    GDirectiveUsages (..),
    applyTypeName,
    applyTypeDescription,
    applyEnumName,
    applyEnumDescription,
    applyFieldName,
    applyFieldDescription,
    applyFieldDefaultValue,
    applyTypeFieldNames,
    applyTypeEnumNames,
    allUsages,
  )
where

{- ORMOLU_DISABLE -}
import qualified Data.HashMap.Strict as M
import qualified Data.Morpheus.Server.Types.Visitors as Visitors
{- ORMOLU_ENABLE -}

import Data.Morpheus.Types.Internal.AST
  ( CONST,
    Description,
    DirectiveLocation (..),
    FALSE,
    FieldName,
    TRUE,
    TypeName,
    Value,
    packName,
    unpackName,
  )
import Relude

type family OR (a :: Bool) (b :: Bool) where
  OR FALSE FALSE = FALSE
  OR a b = TRUE

type family INCLUDES (x :: DirectiveLocation) (xs :: [DirectiveLocation]) :: Bool where
  INCLUDES x '[] = FALSE
  INCLUDES x (x ': xs) = TRUE
  INCLUDES x (a ': xs) = INCLUDES x xs

type family OVERLAPS (xs :: [DirectiveLocation]) (ys :: [DirectiveLocation]) :: Bool where
  OVERLAPS (x ': xs) ys = OR (INCLUDES x ys) (OVERLAPS xs ys)
  OVERLAPS '[] ys = FALSE

-- type VisitorOption (k :: DirectiveLocation) (a :: Type) = VisitorContext a (Allow k (ALLOWED_DIRECTIVE_LOCATIONS a))

class ToLocation (l :: DirectiveLocation) where
  toLocation :: f l -> DirectiveLocation

-- types
instance ToLocation 'LOCATION_OBJECT where
  toLocation = const LOCATION_OBJECT

instance ToLocation 'LOCATION_ENUM where
  toLocation = const LOCATION_ENUM

instance ToLocation 'LOCATION_INPUT_OBJECT where
  toLocation = const LOCATION_INPUT_OBJECT

instance ToLocation 'LOCATION_UNION where
  toLocation = const LOCATION_UNION

instance ToLocation 'LOCATION_SCALAR where
  toLocation = const LOCATION_SCALAR

instance ToLocation 'LOCATION_INTERFACE where
  toLocation = const LOCATION_INTERFACE

-- fields, values
instance ToLocation 'LOCATION_INPUT_FIELD_DEFINITION where
  toLocation = const LOCATION_INPUT_FIELD_DEFINITION

instance ToLocation 'LOCATION_ARGUMENT_DEFINITION where
  toLocation = const LOCATION_ARGUMENT_DEFINITION

instance ToLocation 'LOCATION_FIELD_DEFINITION where
  toLocation = const LOCATION_FIELD_DEFINITION

instance ToLocation 'LOCATION_ENUM_VALUE where
  toLocation = const LOCATION_ENUM_VALUE

class ToLocations (k :: [DirectiveLocation]) where
  toLocations :: f k -> [DirectiveLocation]

instance (ToLocation l, ToLocations ls) => ToLocations (l : ls) where
  toLocations _ = toLocation (Proxy @l) : toLocations (Proxy @ls)

instance ToLocations '[] where
  toLocations _ = []

getLocations :: forall f a. (ToLocations (DIRECTIVE_LOCATIONS a)) => f a -> [DirectiveLocation]
getLocations _ = toLocations (Proxy :: Proxy (DIRECTIVE_LOCATIONS a))

type ALLOWED (a :: Type) (l :: [DirectiveLocation]) = OVERLAPS l (DIRECTIVE_LOCATIONS a)

type WITH_VISITOR (a :: Type) (f :: Type -> Bool -> Constraint) (l :: [DirectiveLocation]) = f a (ALLOWED a l)

-- types

type TYPE_VISITOR_KIND = '[ 'LOCATION_OBJECT, 'LOCATION_ENUM, 'LOCATION_INPUT_OBJECT, 'LOCATION_UNION, 'LOCATION_SCALAR, 'LOCATION_INTERFACE]

type FIELD_VISITOR_KIND = '[ 'LOCATION_INPUT_FIELD_DEFINITION, 'LOCATION_FIELD_DEFINITION]

type ENUM_VISITOR_KIND = '[ 'LOCATION_ENUM_VALUE]

class
  ( ToLocations (DIRECTIVE_LOCATIONS a),
    Typeable a,
    WITH_VISITOR a VISIT_TYPE TYPE_VISITOR_KIND,
    WITH_VISITOR a VISIT_FIELD FIELD_VISITOR_KIND,
    WITH_VISITOR a VISIT_ENUM ENUM_VISITOR_KIND
  ) =>
  GQLDirective a
  where
  type DIRECTIVE_LOCATIONS a :: [DirectiveLocation]
  excludeFromSchema :: f a -> Bool
  excludeFromSchema _ = False

-- TYPE VISITORS

visitTypeName' :: forall a. (GQLDirective a) => a -> Bool -> TypeName -> TypeName
visitTypeName' = __visitTypeName (Proxy :: Proxy (ALLOWED a TYPE_VISITOR_KIND))

visitTypeDescription' :: forall a. (GQLDirective a) => a -> Maybe Description -> Maybe Description
visitTypeDescription' = __visitTypeDescription (Proxy :: Proxy (ALLOWED a TYPE_VISITOR_KIND))

visitFieldNames' :: forall a. (GQLDirective a) => a -> FieldName -> FieldName
visitFieldNames' = __visitFieldNames (Proxy :: Proxy (ALLOWED a TYPE_VISITOR_KIND))

visitEnumNames' :: forall a. (GQLDirective a) => a -> TypeName -> TypeName
visitEnumNames' = __visitEnumNames (Proxy :: Proxy (ALLOWED a TYPE_VISITOR_KIND))

class VISIT_TYPE a (t :: Bool) where
  __visitTypeName :: f t -> a -> Bool -> TypeName -> TypeName
  __visitTypeDescription :: f t -> a -> Maybe Description -> Maybe Description
  __visitFieldNames :: f t -> a -> FieldName -> FieldName
  __visitEnumNames :: f t -> a -> TypeName -> TypeName

instance VISIT_TYPE a 'False where
  __visitTypeName _ _ _ = id
  __visitTypeDescription _ _ = id
  __visitFieldNames _ _ = id
  __visitEnumNames _ _ = id

instance (Visitors.VisitType a) => VISIT_TYPE a TRUE where
  __visitTypeName _ x isInput name = packName $ Visitors.visitTypeName x isInput (unpackName name)
  __visitTypeDescription _ = Visitors.visitTypeDescription
  __visitFieldNames _ x = packName . Visitors.visitFieldNames x . unpackName
  __visitEnumNames _ x = packName . Visitors.visitEnumNames x . unpackName

-- FIELD VISITORS

visitFieldName' :: forall a. (GQLDirective a) => a -> FieldName -> FieldName
visitFieldName' = __visitFieldName (Proxy :: Proxy (ALLOWED a FIELD_VISITOR_KIND))

visitFieldDescription' :: forall a. (GQLDirective a) => a -> Maybe Description -> Maybe Description
visitFieldDescription' = __visitFieldDescription (Proxy :: Proxy (ALLOWED a FIELD_VISITOR_KIND))

visitFieldDefaultValue' :: forall a. (GQLDirective a) => a -> Maybe (Value CONST) -> Maybe (Value CONST)
visitFieldDefaultValue' = __visitFieldDefaultValue (Proxy :: Proxy (ALLOWED a FIELD_VISITOR_KIND))

class VISIT_FIELD a (t :: Bool) where
  __visitFieldName :: f t -> a -> FieldName -> FieldName
  __visitFieldDescription :: f t -> a -> Maybe Description -> Maybe Description
  __visitFieldDefaultValue :: f t -> a -> Maybe (Value CONST) -> Maybe (Value CONST)

instance VISIT_FIELD a FALSE where
  __visitFieldName _ _ = id
  __visitFieldDescription _ _ = id
  __visitFieldDefaultValue _ _ = id

instance (Visitors.VisitField a) => VISIT_FIELD a TRUE where
  __visitFieldName _ x name = packName $ Visitors.visitFieldName x (unpackName name)
  __visitFieldDescription _ = Visitors.visitFieldDescription
  __visitFieldDefaultValue _ = Visitors.visitFieldDefaultValue

-- VISIT_ENUM

visitEnumName' :: forall a. (GQLDirective a) => a -> TypeName -> TypeName
visitEnumName' = __visitEnumName (Proxy :: Proxy (ALLOWED a ENUM_VISITOR_KIND))

visitEnumDescription' :: forall a. (GQLDirective a) => a -> Maybe Description -> Maybe Description
visitEnumDescription' = __visitEnumDescription (Proxy :: Proxy (ALLOWED a ENUM_VISITOR_KIND))

class VISIT_ENUM a (t :: Bool) where
  __visitEnumName :: f t -> a -> TypeName -> TypeName
  __visitEnumDescription :: f t -> a -> Maybe Description -> Maybe Description

instance VISIT_ENUM a FALSE where
  __visitEnumName _ _ = id
  __visitEnumDescription _ _ = id

instance (Visitors.VisitEnum a) => VISIT_ENUM a TRUE where
  __visitEnumName _ x name = packName $ Visitors.visitEnumName x (unpackName name)
  __visitEnumDescription _ = Visitors.visitEnumDescription

data GDirectiveUsage (gql :: Type -> Constraint) (args :: Type -> Constraint) where
  GDirectiveUsage :: (GQLDirective a, gql a, args a) => a -> GDirectiveUsage gql args

-- apply

applyTypeName :: GDirectiveUsage gql args -> Bool -> TypeName -> TypeName
applyTypeName (GDirectiveUsage x) = visitTypeName' x

applyTypeFieldNames :: GDirectiveUsage gql args -> FieldName -> FieldName
applyTypeFieldNames (GDirectiveUsage x) = visitFieldNames' x

applyTypeEnumNames :: GDirectiveUsage gql args -> TypeName -> TypeName
applyTypeEnumNames (GDirectiveUsage x) = visitEnumNames' x

applyEnumDescription :: GDirectiveUsage gql args -> Maybe Description -> Maybe Description
applyEnumDescription (GDirectiveUsage x) = visitEnumDescription' x

applyEnumName :: GDirectiveUsage gql args -> TypeName -> TypeName
applyEnumName (GDirectiveUsage x) = visitEnumName' x

applyFieldName :: GDirectiveUsage gql args -> FieldName -> FieldName
applyFieldName (GDirectiveUsage x) = visitFieldName' x

applyFieldDescription :: GDirectiveUsage gql args -> Maybe Description -> Maybe Description
applyFieldDescription (GDirectiveUsage x) = visitFieldDescription' x

applyFieldDefaultValue :: GDirectiveUsage gql args -> Maybe (Value CONST) -> Maybe (Value CONST)
applyFieldDefaultValue (GDirectiveUsage x) = visitFieldDefaultValue' x

applyTypeDescription :: GDirectiveUsage gql args -> Maybe Description -> Maybe Description
applyTypeDescription (GDirectiveUsage x) = visitTypeDescription' x

data GDirectiveUsages gql args = GDirectiveUsages
  { typeDirectives :: [GDirectiveUsage gql args],
    fieldDirectives :: M.HashMap FieldName [GDirectiveUsage gql args],
    enumValueDirectives :: M.HashMap TypeName [GDirectiveUsage gql args]
  }

allUsages :: GDirectiveUsages gql args -> [GDirectiveUsage gql args]
allUsages GDirectiveUsages {..} =
  join (toList enumValueDirectives)
    <> join (toList fieldDirectives)
    <> typeDirectives

instance Monoid (GDirectiveUsages gql args) where
  mempty = GDirectiveUsages mempty mempty mempty

instance Semigroup (GDirectiveUsages gql args) where
  GDirectiveUsages td1 fd1 ed1 <> GDirectiveUsages td2 fd2 ed2 =
    GDirectiveUsages (td1 <> td2) (mergeDirs fd1 fd2) (mergeDirs ed1 ed2)

mergeDirs :: (Eq k, Hashable k, Semigroup v) => HashMap k v -> HashMap k v -> HashMap k v
mergeDirs a b = update a (M.toList b)
  where
    update m [] = m
    update m (x : xs) = update (upsert x m) xs

upsert :: (Eq k, Hashable k, Semigroup v) => (k, v) -> HashMap k v -> HashMap k v
upsert (k, v) = M.alter (Just . maybe v (v <>)) k
