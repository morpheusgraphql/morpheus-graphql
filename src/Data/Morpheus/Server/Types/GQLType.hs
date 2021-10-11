{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.GQLType
  ( GQLType
      ( KIND,
        description,
        getDescriptions,
        typeOptions,
        getDirectives,
        defaultValues,
        __type
      ),
    GQLTypeOptions (..),
    defaultTypeOptions,
    TypeData (..),
    __isEmptyType,
    __typeData,
  )
where

-- MORPHEUS

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    SubscriptionField,
  )
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.NamedResolvers (NamedResolverT (..))
import Data.Morpheus.Server.Deriving.Utils.Kinded (CategoryValue (..))
import Data.Morpheus.Server.Types.SchemaT
  ( TypeFingerprint (..),
  )
import Data.Morpheus.Server.Types.Types
  ( Arg,
    Pair,
    TypeGuard,
    Undefined (..),
  )
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    Description,
    Directives,
    TypeCategory (..),
    TypeName,
    TypeWrapper (..),
    Value,
    mkBaseType,
    packName,
    toNullable,
    unpackName,
  )
import Data.Text
  ( intercalate,
    pack,
    unpack,
  )
import Data.Typeable
  ( TyCon,
    TypeRep,
    splitTyConApp,
    tyConFingerprint,
    tyConName,
    typeRep,
    typeRepTyCon,
  )
import Relude hiding (Undefined, intercalate)

data TypeData = TypeData
  { gqlTypeName :: TypeName,
    gqlWrappers :: TypeWrapper,
    gqlFingerprint :: TypeFingerprint
  }
  deriving (Show)

-- | Options that specify how to map GraphQL field, type, and constructor names
-- to and from their Haskell equivalent.
--
-- Options can be set using record syntax on 'defaultOptions' with the fields
-- below.
data GQLTypeOptions = GQLTypeOptions
  { -- | Function applied to field labels.
    -- Handy for removing common record prefixes for example.
    fieldLabelModifier :: String -> String,
    -- | Function applied to constructor tags.
    constructorTagModifier :: String -> String,
    -- | Construct a new type name depending on whether it is an input,
    -- and being given the original type name.
    typeNameModifier :: Bool -> String -> String
  }

-- | Default encoding 'GQLTypeOptions':
--
-- @
-- 'GQLTypeOptions'
--   { 'fieldLabelModifier'      = id
--   , 'constructorTagModifier'  = id
--   , 'typeNameModifier'        = const id
--   }
-- @
defaultTypeOptions :: GQLTypeOptions
defaultTypeOptions =
  GQLTypeOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id,
      -- default is just a pass through for the original type name
      typeNameModifier = const id
    }

__typeData ::
  forall kinded (kind :: TypeCategory) (a :: *).
  (GQLType a, CategoryValue kind) =>
  kinded kind a ->
  TypeData
__typeData proxy = __type proxy (categoryValue (Proxy @kind))

getTypename :: Typeable a => f a -> TypeName
getTypename = packName . intercalate "" . getTypeConstructorNames

getTypeConstructorNames :: Typeable a => f a -> [Text]
getTypeConstructorNames = fmap (pack . tyConName . replacePairCon) . getTypeConstructors

getTypeConstructors :: Typeable a => f a -> [TyCon]
getTypeConstructors = ignoreResolver . splitTyConApp . typeRep

deriveTypeData :: Typeable a => f a -> (Bool -> String -> String) -> TypeCategory -> TypeData
deriveTypeData proxy typeNameModifier cat =
  TypeData
    { gqlTypeName = packName . pack $ typeNameModifier (cat == IN) originalTypeName,
      gqlWrappers = mkBaseType,
      gqlFingerprint = getFingerprint cat proxy
    }
  where
    originalTypeName = unpack . unpackName $ getTypename proxy

getFingerprint :: Typeable a => TypeCategory -> f a -> TypeFingerprint
getFingerprint category = TypeableFingerprint category . fmap tyConFingerprint . getTypeConstructors

mkTypeData :: TypeName -> a -> TypeData
mkTypeData name _ =
  TypeData
    { gqlTypeName = name,
      gqlFingerprint = InternalFingerprint name,
      gqlWrappers = mkBaseType
    }

list :: TypeWrapper -> TypeWrapper
list = flip TypeList True

wrapper :: (TypeWrapper -> TypeWrapper) -> TypeData -> TypeData
wrapper f TypeData {..} = TypeData {gqlWrappers = f gqlWrappers, ..}

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x | hsPair == x = gqlPair
  where
    hsPair = typeRepTyCon $ typeRep $ Proxy @(Int, Int)
    gqlPair = typeRepTyCon $ typeRep $ Proxy @(Pair Int Int)
replacePairCon x = x

-- Ignores Resolver name  from typeName
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _) | con == typeRepTyCon (typeRep $ Proxy @Resolver) = []
ignoreResolver (con, _) | con == typeRepTyCon (typeRep $ Proxy @NamedResolverT) = []
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
class GQLType a where
  type KIND a :: DerivingKind
  type KIND a = TYPE

  -- | A description of the type.
  --
  -- Used for documentation in the GraphQL schema.
  description :: f a -> Maybe Text
  description _ = Nothing

  -- | A dictionary of descriptions for fields, keyed on field name.
  --
  -- Used for documentation in the GraphQL schema.
  getDescriptions :: f a -> Map Text Description
  getDescriptions _ = mempty

  typeOptions :: f a -> GQLTypeOptions -> GQLTypeOptions
  typeOptions _ = id

  getDirectives :: f a -> Map Text (Directives CONST)
  getDirectives _ = mempty

  defaultValues :: f a -> Map Text (Value CONST)
  defaultValues _ = mempty

  __isEmptyType :: f a -> Bool
  __isEmptyType _ = False

  __type :: f a -> TypeCategory -> TypeData
  default __type :: Typeable a => f a -> TypeCategory -> TypeData
  __type proxy = deriveTypeData proxy typeNameModifier
    where
      GQLTypeOptions {typeNameModifier} = typeOptions proxy defaultTypeOptions

instance GQLType Int where
  type KIND Int = SCALAR
  __type _ = mkTypeData "Int"

instance GQLType Double where
  type KIND Double = SCALAR
  __type _ = mkTypeData "Float"

instance GQLType Float where
  type KIND Float = SCALAR
  __type _ = mkTypeData "Float32"

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

instance GQLType a => GQLType (Maybe a) where
  type KIND (Maybe a) = WRAPPER
  __type _ = wrapper toNullable . __type (Proxy @a)

instance GQLType a => GQLType [a] where
  type KIND [a] = WRAPPER
  __type _ = wrapper list . __type (Proxy @a)

instance GQLType a => GQLType (Set a) where
  type KIND (Set a) = WRAPPER
  __type _ = __type $ Proxy @[a]

instance GQLType a => GQLType (SubscriptionField a) where
  type KIND (SubscriptionField a) = WRAPPER
  __type _ = __type $ Proxy @a

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b)

-- Manual

instance Typeable m => GQLType (Undefined m) where
  type KIND (Undefined m) = CUSTOM
  __isEmptyType _ = True

instance GQLType b => GQLType (a -> b) where
  type KIND (a -> b) = CUSTOM
  __type _ = __type $ Proxy @b

instance (GQLType k, GQLType v, Typeable k, Typeable v) => GQLType (Map k v) where
  type KIND (Map k v) = CUSTOM
  __type _ = __type $ Proxy @[Pair k v]

instance GQLType a => GQLType (Resolver o e m a) where
  type KIND (Resolver o e m a) = CUSTOM
  __type _ = __type $ Proxy @a

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (a, b) where
  type KIND (a, b) = CUSTOM
  __type _ = __type $ Proxy @(Pair a b)

instance (GQLType value) => GQLType (Arg name value) where
  type KIND (Arg name value) = CUSTOM
  __type _ = __type (Proxy @value)

instance (GQLType interface) => GQLType (TypeGuard interface possibleTypes) where
  type KIND (TypeGuard interface possibleTypes) = CUSTOM
  __type _ = __type (Proxy @interface)

instance (GQLType a) => GQLType (Proxy a) where
  type KIND (Proxy a) = KIND a
  __type _ = __type (Proxy @a)

instance (GQLType a) => GQLType (NamedResolverT m a) where
  type KIND (NamedResolverT m a) = CUSTOM
  __type _ = __type (Proxy :: Proxy a)
