{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Channels
  ( --  Introspect (..),
    --   DeriveTypeContent (..),
    --   introspectOUT,
    --   IntroCon,
    --   updateLib,
    --   buildType,
    --   introspectObjectFields,
    --   deriveCustomInputObjectType,
    -- TypeUpdater,
    GetChannel (..),
  )
where

import Data.List (partition)
import Data.Map (Map)
-- MORPHEUS

import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( concatUpdates,
    empty,
    failUpdates,
    singleton,
  )
import Data.Morpheus.Kind
  ( Context (..),
    ENUM,
    GQL_KIND,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( EnumRep (..),
    conNameProxy,
    isRecordProxy,
    selNameProxy,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeUpdater,
  )
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair,
  )
import Data.Morpheus.Types.GQLScalar (GQLScalar (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    DataFingerprint (..),
    DataUnion,
    FALSE,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldName (..),
    FieldsDefinition,
    IN,
    Message,
    OUT,
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
    fieldsToArguments,
    insertType,
    mkEnumContent,
    mkInputValue,
    mkTypeRef,
    mkUnionMember,
    msg,
    toListField,
    toNullable,
    unsafeFromFields,
    updateSchema,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text
  ( pack,
  )
import GHC.Generics

data FieldChannel = FieldChannel String | Object [(FieldName, FieldChannel)]

data WithChannel e m a = WithChannel
  { channel :: String,
    resolver :: e -> m a
  }

class GetChannel a where
  getChannel :: a -> FieldChannel

instance {-# OVERLAPPABLE #-} (EnumRep (Rep a)) => GetChannel a where
  getChannel = Object $ typeRep (from @(Rep a))

instance GetChannel (WithChannel e m a) where
  getChannel WithChannel {channel} = FieldChannel channel

class TypeRep f where
  typeRep :: f a -> [(FieldName, FieldChannel)]

instance TypeRep f => TypeRep (M1 D d f) where
  typeRep (M1 src) = typeRep src

instance (Constructor c) => TypeRep (M1 C c f) where
  typeRep context (M1 src) = fieldRep src

--- FIELDS
class FieldRep f where
  fieldRep :: f a -> [(FieldName, FieldChannel)]

instance (FieldRep f, FieldRep g) => FieldRep (f :*: g) where
  fieldRep (a :*: b) = fieldRep a <> fieldRep b

instance (Selector s, GQLType a, GetChannel a) => FieldRep (M1 S s (K1 s2 a)) where
  fieldRep m@(M1 (K1 src)) = [(FieldName $ pack (selName m), getChannel src)]

instance FieldRep U1 where
  fieldRep _ = []
