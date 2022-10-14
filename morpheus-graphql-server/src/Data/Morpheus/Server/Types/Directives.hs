{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
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
    -- visitors
    visitTypeName',
    visitTypeDescription',
    visitFieldName',
    visitFieldDescription',
    visitEnumName',
    visitEnumDescription',
    visitFieldNames',
    visitEnumNames',
  )
where

import Data.Morpheus.Server.Types.TypeName (getTypename)
import Data.Morpheus.Server.Types.Visitors qualified as Visitors
import Data.Morpheus.Types.Internal.AST
  ( Description,
    DirectiveLocation (..),
    FALSE,
    FieldName,
    TRUE,
    TypeName,
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
instance ToLocation 'OBJECT where
  toLocation = const OBJECT

instance ToLocation 'ENUM where
  toLocation = const ENUM

instance ToLocation 'INPUT_OBJECT where
  toLocation = const INPUT_OBJECT

instance ToLocation 'UNION where
  toLocation = const UNION

instance ToLocation 'SCALAR where
  toLocation = const SCALAR

instance ToLocation 'INTERFACE where
  toLocation = const INTERFACE

-- fields, values
instance ToLocation 'INPUT_FIELD_DEFINITION where
  toLocation = const INPUT_FIELD_DEFINITION

instance ToLocation 'ARGUMENT_DEFINITION where
  toLocation = const ARGUMENT_DEFINITION

instance ToLocation 'FIELD_DEFINITION where
  toLocation = const FIELD_DEFINITION

instance ToLocation 'ENUM_VALUE where
  toLocation = const ENUM_VALUE

class ToLocations (k :: [DirectiveLocation]) where
  toLocations :: f k -> [DirectiveLocation]

instance (ToLocation l, ToLocations ls) => ToLocations (l : ls) where
  toLocations _ = toLocation (Proxy @l) : toLocations (Proxy @ls)

instance ToLocations '[] where
  toLocations _ = []

getLocations :: forall f a. ToLocations (DIRECTIVE_LOCATIONS a) => f a -> [DirectiveLocation]
getLocations _ = toLocations (Proxy :: Proxy (DIRECTIVE_LOCATIONS a))

type ALLOWED (a :: Type) (l :: [DirectiveLocation]) = OVERLAPS l (DIRECTIVE_LOCATIONS a)

type WITH_VISITOR (a :: Type) (f :: Type -> Bool -> Constraint) (l :: [DirectiveLocation]) = f a (ALLOWED a l)

-- types

type TYPE_VISITOR_KIND = '[ 'OBJECT, 'ENUM, 'INPUT_OBJECT, 'UNION, 'SCALAR, 'INTERFACE]

type FIELD_VISITOR_KIND = '[ 'INPUT_FIELD_DEFINITION, 'FIELD_DEFINITION]

type ENUM_VISITOR_KIND = '[ 'ENUM_VALUE]

__directiveName :: GQLDirective a => f a -> FieldName
__directiveName = coerce . getTypename

class
  ( Typeable a,
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

visitTypeName' :: forall a. GQLDirective a => a -> Bool -> TypeName -> TypeName
visitTypeName' = __visitTypeName (Proxy :: Proxy (ALLOWED a TYPE_VISITOR_KIND))

visitTypeDescription' :: forall a. GQLDirective a => a -> Maybe Description -> Maybe Description
visitTypeDescription' = __visitTypeDescription (Proxy :: Proxy (ALLOWED a TYPE_VISITOR_KIND))

visitFieldNames' :: forall a. GQLDirective a => a -> FieldName -> FieldName
visitFieldNames' = __visitFieldNames (Proxy :: Proxy (ALLOWED a TYPE_VISITOR_KIND))

visitEnumNames' :: forall a. GQLDirective a => a -> TypeName -> TypeName
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

instance Visitors.VisitType a => VISIT_TYPE a TRUE where
  __visitTypeName _ x isInput name = packName $ Visitors.visitTypeName x isInput (unpackName name)
  __visitTypeDescription _ = Visitors.visitTypeDescription
  __visitFieldNames _ x = packName . Visitors.visitFieldNames x . unpackName
  __visitEnumNames _ x = packName . Visitors.visitEnumNames x . unpackName

-- FIELD VISITORS

visitFieldName' :: forall a. GQLDirective a => a -> FieldName -> FieldName
visitFieldName' = __visitFieldName (Proxy :: Proxy (ALLOWED a FIELD_VISITOR_KIND))

visitFieldDescription' :: forall a. GQLDirective a => a -> Maybe Description -> Maybe Description
visitFieldDescription' = __visitFieldDescription (Proxy :: Proxy (ALLOWED a FIELD_VISITOR_KIND))

class VISIT_FIELD a (t :: Bool) where
  __visitFieldName :: f t -> a -> FieldName -> FieldName
  __visitFieldDescription :: f t -> a -> Maybe Description -> Maybe Description

instance VISIT_FIELD a FALSE where
  __visitFieldName _ _ = id
  __visitFieldDescription _ _ = id

instance Visitors.VisitField a => VISIT_FIELD a TRUE where
  __visitFieldName _ x name = packName $ Visitors.visitFieldName x (unpackName name)
  __visitFieldDescription _ = Visitors.visitFieldDescription

-- VISIT_ENUM

visitEnumName' :: forall a. GQLDirective a => a -> TypeName -> TypeName
visitEnumName' = __visitEnumName (Proxy :: Proxy (ALLOWED a ENUM_VISITOR_KIND))

visitEnumDescription' :: forall a. GQLDirective a => a -> Maybe Description -> Maybe Description
visitEnumDescription' = __visitEnumDescription (Proxy :: Proxy (ALLOWED a ENUM_VISITOR_KIND))

class VISIT_ENUM a (t :: Bool) where
  __visitEnumName :: f t -> a -> TypeName -> TypeName
  __visitEnumDescription :: f t -> a -> Maybe Description -> Maybe Description

instance VISIT_ENUM a FALSE where
  __visitEnumName _ _ = id
  __visitEnumDescription _ _ = id

instance Visitors.VisitEnum a => VISIT_ENUM a TRUE where
  __visitEnumName _ x name = packName $ Visitors.visitEnumName x (unpackName name)
  __visitEnumDescription _ = Visitors.visitEnumDescription
