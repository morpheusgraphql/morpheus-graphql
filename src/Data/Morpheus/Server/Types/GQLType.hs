{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeUpdater,
  )
where

-- MORPHEUS

import Data.Map (Map)
import Data.Morpheus.Internal.Utils (UpdateT)
import Data.Morpheus.Kind
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair,
    Undefined (..),
  )
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    DataFingerprint (..),
    Description,
    Directive,
    FieldName,
    QUERY,
    Schema,
    TypeName (..),
    Value,
    internalFingerprint,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Resolver,
    SubscriptionField,
  )
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text
  ( Text,
    intercalate,
    pack,
  )
import Data.Typeable
  ( TyCon,
    TypeRep,
    Typeable,
    splitTyConApp,
    tyConFingerprint,
    tyConName,
    typeRep,
    typeRepTyCon,
  )
import Prelude
  ( ($),
    (.),
    (<$>),
    Bool (..),
    Either,
    Eq (..),
    Float,
    Int,
    Maybe (..),
    concatMap,
    fmap,
    mempty,
    show,
  )

type TypeUpdater = UpdateT Eventless (Schema CONST)

resolverCon :: TyCon
resolverCon = typeRepTyCon $ typeRep $ Proxy @(Resolver QUERY () Maybe)

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x | hsPair == x = gqlPair
  where
    hsPair = typeRepTyCon $ typeRep $ Proxy @(Int, Int)
    gqlPair = typeRepTyCon $ typeRep $ Proxy @(Pair Int Int)
replacePairCon x = x

-- Ignores Resolver name  from typeName
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _) | con == resolverCon = []
ignoreResolver (con, args) =
  con : concatMap (ignoreResolver . splitTyConApp) args

-- | GraphQL type, every graphQL type should have an instance of 'GHC.Generics.Generic' and 'GQLType'.
--
--  @
--    ... deriving (Generic, GQLType)
--  @
--
-- if you want to add description
--
--  @
--       ... deriving (Generic)
--
--     instance GQLType ... where
--       description = const "your description ..."
--  @
class IsObject (a :: GQL_KIND) where
  isObject :: Proxy a -> Bool

instance IsObject SCALAR where
  isObject _ = False

instance IsObject ENUM where
  isObject _ = False

instance IsObject WRAPPER where
  isObject _ = False

instance IsObject INPUT where
  isObject _ = True

instance IsObject OUTPUT where
  isObject _ = True

instance IsObject INTERFACE where
  isObject _ = True

class IsObject (KIND a) => GQLType a where
  type KIND a :: GQL_KIND
  type KIND a = OUTPUT

  implements :: f a -> [(TypeName, TypeUpdater)]
  implements _ = []

  description :: f a -> Maybe Text
  description _ = mempty

  isObjectKind :: f a -> Bool
  isObjectKind _ = isObject (Proxy @(KIND a))

  hasNamespace :: f a -> Maybe TypeName
  hasNamespace _ = mempty

  fieldDescriptions :: f a -> Map FieldName Description
  fieldDescriptions _ = mempty

  isEmptyType :: f a -> Bool
  isEmptyType _ = False

  fieldValues ::
    f a ->
    Map
      FieldName
      ( Maybe Description, --description
        [Directive CONST], -- directives
        Maybe (Value CONST), -- defaultValue,
        Maybe (ArgumentsDefinition CONST)
      )
  fieldValues _ = mempty

  __typeName :: f a -> TypeName
  default __typeName ::
    (Typeable a) =>
    f a ->
    TypeName
  __typeName _ = TypeName $ intercalate "_" (getName $ Proxy @a)
    where
      getName = fmap (fmap (pack . tyConName)) (fmap replacePairCon . ignoreResolver . splitTyConApp . typeRep)

  __typeFingerprint :: f a -> DataFingerprint
  default __typeFingerprint ::
    (Typeable a) =>
    f a ->
    DataFingerprint
  __typeFingerprint _ = DataFingerprint "Typeable" $ show <$> conFingerprints (Proxy @a)
    where
      conFingerprints = fmap (fmap tyConFingerprint) (ignoreResolver . splitTyConApp . typeRep)

instance GQLType () where
  type KIND () = WRAPPER

instance Typeable m => GQLType (Undefined m) where
  type KIND (Undefined m) = WRAPPER
  isEmptyType _ = True

instance GQLType Int where
  type KIND Int = SCALAR
  __typeFingerprint _ = internalFingerprint "Int" []

instance GQLType Float where
  type KIND Float = SCALAR
  __typeFingerprint _ = internalFingerprint "Float" []

instance GQLType Text where
  type KIND Text = SCALAR
  __typeName _ = "String"
  __typeFingerprint _ = internalFingerprint "String" []

instance GQLType Bool where
  type KIND Bool = SCALAR
  __typeName _ = "Boolean"
  __typeFingerprint _ = internalFingerprint "Boolean" []

instance GQLType a => GQLType (Maybe a) where
  type KIND (Maybe a) = WRAPPER
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType [a] where
  type KIND [a] = WRAPPER
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (a, b) where
  type KIND (a, b) = WRAPPER
  __typeName _ = __typeName $ Proxy @(Pair a b)

instance GQLType a => GQLType (Set a) where
  type KIND (Set a) = WRAPPER
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b) where
  type KIND (Pair a b) = OUTPUT

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (MapKind a b m) where
  type KIND (MapKind a b m) = OUTPUT
  __typeName _ = __typeName (Proxy @(Map a b))
  __typeFingerprint _ = __typeFingerprint (Proxy @(Map a b))

instance (Typeable k, Typeable v) => GQLType (Map k v) where
  type KIND (Map k v) = WRAPPER

instance GQLType a => GQLType (Either s a) where
  type KIND (Either s a) = WRAPPER
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType (Resolver o e m a) where
  type KIND (Resolver o e m a) = WRAPPER
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType (SubscriptionField a) where
  type KIND (SubscriptionField a) = WRAPPER
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType b => GQLType (a -> b) where
  type KIND (a -> b) = WRAPPER
  __typeName _ = __typeName (Proxy @b)
  __typeFingerprint _ = __typeFingerprint (Proxy @b)

instance GQLType ID where
  type KIND ID = SCALAR
  __typeFingerprint _ = internalFingerprint "ID" []
