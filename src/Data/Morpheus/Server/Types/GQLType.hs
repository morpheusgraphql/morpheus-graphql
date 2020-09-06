{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLTypeOptions (..),
    defaultTypeOptions,
  )
where

-- MORPHEUS
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
    TypeRef (..),
    TypeWrapper (..),
    Value,
    internalFingerprint,
    mkTypeRef,
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
    String,
    concatMap,
    fmap,
    id,
    mempty,
    show,
  )

data TypeData = TypeData
  { gqlRef :: TypeRef,
    gqlFingerprint :: DataFingerprint
  }

data GQLTypeOptions = GQLTypeOptions
  { fieldLabelModifier :: String -> String,
    constructorTagModifier :: String -> String
  }

defaultTypeOptions :: GQLTypeOptions
defaultTypeOptions =
  GQLTypeOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }

getTypename :: forall f a. Typeable a => f a -> TypeName
getTypename _ = TypeName $ intercalate "_" (getName $ Proxy @a)
  where
    getName = fmap (fmap (pack . tyConName)) (fmap replacePairCon . ignoreResolver . splitTyConApp . typeRep)

getFingerprint :: forall f a. Typeable a => f a -> DataFingerprint
getFingerprint _ = DataFingerprint "Typeable" $ show <$> conFingerprints (Proxy @a)
  where
    conFingerprints = fmap (fmap tyConFingerprint) (ignoreResolver . splitTyConApp . typeRep)

deriveTypeData :: Typeable a => f a -> TypeData
deriveTypeData proxy =
  TypeData
    { gqlRef = mkTypeRef (getTypename proxy),
      gqlFingerprint = getFingerprint proxy
    }

mkTypeData :: TypeName -> TypeData
mkTypeData name =
  TypeData
    { gqlRef = mkTypeRef name,
      gqlFingerprint = internalFingerprint "Int" []
    }

list :: [TypeWrapper] -> [TypeWrapper]
list = (TypeList :)

wrapper :: ([TypeWrapper] -> [TypeWrapper]) -> TypeData -> TypeData
wrapper f TypeData {gqlRef = TypeRef {..}, ..} =
  TypeData {gqlRef = TypeRef {typeWrappers = f typeWrappers, ..}, ..}

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

  description :: f a -> Maybe Text
  description _ = Nothing

  getDescriptions :: f a -> Map Text Description
  getDescriptions _ = mempty

  typeOptions :: f a -> GQLTypeOptions
  typeOptions _ = defaultTypeOptions

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

  __type :: f a -> TypeData
  default __type :: (Typeable a) => f a -> TypeData
  __type _ = deriveTypeData (Proxy @a)

instance GQLType Int where
  type KIND Int = SCALAR
  __type _ = mkTypeData "Int"

instance GQLType Float where
  type KIND Float = SCALAR
  __type _ = mkTypeData "Float"

instance GQLType Text where
  type KIND Text = SCALAR
  __type _ = mkTypeData "String"

instance GQLType Bool where
  type KIND Bool = SCALAR
  __type _ = mkTypeData "Boolean"

instance GQLType ID where
  type KIND ID = SCALAR
  __type _ = mkTypeData "ID"

-- WRAPPERS
instance GQLType ()

instance Typeable m => GQLType (Undefined m) where
  type KIND (Undefined m) = WRAPPER
  isEmptyType _ = True

instance GQLType a => GQLType (Maybe a) where
  type KIND (Maybe a) = WRAPPER
  __type _ = wrapper toNullable $ __type (Proxy @a)

instance GQLType a => GQLType [a] where
  type KIND [a] = WRAPPER
  __type _ = wrapper list $ __type (Proxy @a)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (a, b) where
  type KIND (a, b) = WRAPPER
  __type _ = __type (Proxy @(Pair a b))

instance GQLType a => GQLType (Set a) where
  type KIND (Set a) = WRAPPER
  __type _ = __type (Proxy @[a])

instance (Typeable k, Typeable v) => GQLType (Map k v) where
  type KIND (Map k v) = WRAPPER

instance GQLType a => GQLType (Resolver o e m a) where
  type KIND (Resolver o e m a) = WRAPPER
  __type _ = __type (Proxy @a)

instance GQLType a => GQLType (SubscriptionField a) where
  type KIND (SubscriptionField a) = WRAPPER
  __type _ = __type (Proxy @a)

instance GQLType b => GQLType (a -> b) where
  type KIND (a -> b) = WRAPPER
  __type _ = __type (Proxy @b)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (MapKind a b m) where
  __type _ = __type (Proxy @(Map a b))
