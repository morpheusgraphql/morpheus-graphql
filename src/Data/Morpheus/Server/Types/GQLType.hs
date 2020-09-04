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
  )
where

-- MORPHEUS
import Control.Applicative (Applicative (..))
import Data.Map (Map)
import Data.Morpheus.Kind
import Data.Morpheus.Server.Types.SchemaT (SchemaT)
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
    Directives,
    FieldName,
    QUERY,
    TypeName (..),
    TypeWrapper (..),
    Value,
    internalFingerprint,
    toNullable,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Resolver,
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
    Eq (..),
    Float,
    Int,
    Maybe (..),
    concatMap,
    fmap,
    mempty,
    show,
  )

wrapList :: [TypeWrapper] -> [TypeWrapper]
wrapList = (TypeList :)

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

  implements :: f a -> [SchemaT TypeName]
  implements _ = []

  isObjectKind :: f a -> Bool
  isObjectKind _ = isObject (Proxy @(KIND a))

  getNamespace :: f a -> Maybe TypeName
  getNamespace _ = Nothing

  description :: f a -> Maybe Text
  description _ = Nothing

  getDescriptions :: f a -> Map Text Description
  getDescriptions _ = mempty

  getDirectives :: f a -> Map Text (Directives CONST)
  getDirectives _ = mempty

  getFieldContents ::
    f a ->
    Map
      FieldName
      ( Maybe (Value CONST),
        Maybe (ArgumentsDefinition CONST)
      )
  getFieldContents _ = mempty

  isEmptyType :: f a -> Bool
  isEmptyType _ = False

  __wrappers :: f a -> [TypeWrapper]
  __wrappers _ = []

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

instance GQLType ID where
  type KIND ID = SCALAR
  __typeFingerprint _ = internalFingerprint "ID" []

-- WRAPPERS
instance GQLType ()

instance Typeable m => GQLType (Undefined m) where
  type KIND (Undefined m) = WRAPPER
  isEmptyType _ = True

instance GQLType a => GQLType (Maybe a) where
  type KIND (Maybe a) = WRAPPER
  __wrappers _ = toNullable $ __wrappers $ Proxy @a
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint $ Proxy @a

instance GQLType a => GQLType [a] where
  type KIND [a] = WRAPPER
  __wrappers _ = wrapList $ __wrappers $ Proxy @a
  __typeName _ = __typeName $ Proxy @a
  __typeFingerprint _ = __typeFingerprint $ Proxy @a

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (a, b) where
  type KIND (a, b) = WRAPPER
  __typeName _ = __typeName $ Proxy @(Pair a b)

instance GQLType a => GQLType (Set a) where
  type KIND (Set a) = WRAPPER
  __wrappers _ = __wrappers $ Proxy @[a]
  __typeName _ = __typeName $ Proxy @a
  __typeFingerprint _ = __typeFingerprint $ Proxy @a

instance (Typeable k, Typeable v) => GQLType (Map k v) where
  type KIND (Map k v) = WRAPPER

instance GQLType a => GQLType (Resolver o e m a) where
  type KIND (Resolver o e m a) = WRAPPER
  __wrappers _ = __wrappers $ Proxy @a
  __typeName _ = __typeName $ Proxy @a
  __typeFingerprint _ = __typeFingerprint $ Proxy @a

instance GQLType a => GQLType (SubscriptionField a) where
  type KIND (SubscriptionField a) = WRAPPER
  __wrappers _ = __wrappers $ Proxy @a
  __typeName _ = __typeName $ Proxy @a
  __typeFingerprint _ = __typeFingerprint $ Proxy @a

instance GQLType b => GQLType (a -> b) where
  type KIND (a -> b) = WRAPPER
  __wrappers _ = __wrappers $ Proxy @b
  __typeName _ = __typeName $ Proxy @b
  __typeFingerprint _ = __typeFingerprint $ Proxy @b

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (MapKind a b m) where
  __typeName _ = __typeName $ Proxy @(Map a b)
  __typeFingerprint _ = __typeFingerprint $ Proxy @(Map a b)
