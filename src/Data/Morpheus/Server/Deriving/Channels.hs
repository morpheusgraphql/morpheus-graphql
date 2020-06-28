{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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

-- type IntroCon a = (GQLType a, DeriveTypeContent OUT (CUSTOM a) a)

-- data ProxyRep (cat :: TypeCategory) a
--   = ProxyRep

-- introspectOUT :: forall a. (GQLType a, Introspect OUT a) => Proxy a -> TypeUpdater
-- introspectOUT _ = introspect (ProxyRep :: ProxyRep OUT a)

-- instance {-# OVERLAPPABLE #-} (GQLType a, IntrospectKind (KIND a) a) => Introspect cat a where
--   introspect _ = introspectKind (Context :: Context (KIND a) a)

data FieldChannel = FieldChannel FieldName String | Object [(String, FieldChannel)]

data WithChannel e m a = WithChannel
  { channel :: String,
    resolver :: e -> m a
  }

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class GetChannel a where
  getChannel :: FieldName -> a -> FieldChannel

instance {-# OVERLAPPABLE #-} (EnumRep (Rep a)) => GetChannel a where
  getChannel name = dericeChannelRep (Context :: Context (KIND a) a)

instance GetChannel (WithChannel e m a) where
  getChannel name WithChannel {channel} = FieldChannel name channel

class DericeChannelRep f where
  dericeChannelRep :: f -> [(String, FieldChannel)]

instance DericeChannelRep U1 where
  dericeChannelRep _ = []

instance DericeChannelRep cat f => DericeChannelRep cat (M1 D d f) where
  dericeChannelRep _ = typeRep (ProxyRep :: ProxyRep cat f)

instance (ConRep cat f, Constructor c) => TypeRep cat (M1 C c f) where
  typeRep _ =
    [ ConsRep
        { consName = conNameProxy (Proxy @c),
          consFields = conRep (ProxyRep :: ProxyRep cat f),
          consIsRecord = isRecordProxy (Proxy @c)
        }
    ]

-- class ConRep cat f where
--   conRep :: ProxyRep cat f -> [FieldRep cat]

-- -- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
-- instance (ConRep cat a, ConRep cat b) => ConRep cat (a :*: b) where
--   conRep _ = conRep (ProxyRep :: ProxyRep cat a) <> conRep (ProxyRep :: ProxyRep cat b)

-- instance (Selector s, Introspect cat a) => ConRep cat (M1 S s (Rec0 a)) where
--   conRep _ =
--     [ FieldRep
--         { fieldTypeName = typeConName $ fieldType fieldData,
--           fieldData = fieldData,
--           fieldTypeUpdater = introspect (ProxyRep :: ProxyRep cat a),
--           fieldIsObject = isObject (ProxyRep :: ProxyRep cat a)
--         }
--     ]
--     where
--       name = selNameProxy (Proxy @s)
--       fieldData = field (ProxyRep :: ProxyRep cat a) name
