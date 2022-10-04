{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.GQLType
  ( GQLType
      ( KIND,
        description,
        getDescriptions,
        typeOptions,
        getDirectives,
        defaultValues,
        directives,
        __type,
        __isEmptyType
      ),
    __typeData,
    deriveTypename,
    deriveFingerprint,
    encodeArguments,
    DirectiveUsage (..),
    DeriveArguments (..),
    DirectiveUsages (..),
    typeDirective,
    fieldDirective,
    enumDirective,
    applyOnTypeName,
    applyEnumDescription,
    applyFieldDescription,
    applyTypeDescription,
  )
where

-- MORPHEUS

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as M
import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    SubscriptionField,
  )
import Data.Morpheus.Internal.Ext
import Data.Morpheus.Internal.Utils
import Data.Morpheus.Server.Deriving.Utils (ConsRep (..), DataType (..), DeriveWith, FieldRep (..))
import Data.Morpheus.Server.Deriving.Utils.DeriveGType (DeriveValueOptions (..), deriveValue)
import Data.Morpheus.Server.Deriving.Utils.Kinded (CategoryValue (..), KindedProxy (KindedProxy), kinded)
import Data.Morpheus.Server.Deriving.Utils.Proxy (ContextValue (..))
import Data.Morpheus.Server.NamedResolvers (NamedResolverT (..))
import Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
    ToLocations,
    visitEnumDescription,
    visitFieldDescription,
    visitTypeDescription,
    visitTypeName,
  )
import Data.Morpheus.Server.Types.Internal
  ( GQLTypeOptions (..),
    TypeData (..),
    defaultTypeOptions,
    mkTypeData,
    prefixInputs,
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind (..),
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.SchemaT (SchemaT)
import Data.Morpheus.Server.Types.TypeName
  ( TypeFingerprint,
    getFingerprint,
    getTypename,
  )
import Data.Morpheus.Server.Types.Types
  ( Arg,
    Pair,
    TypeGuard,
    Undefined (..),
  )
import Data.Morpheus.Types.GQLScalar (EncodeScalar (..))
import Data.Morpheus.Types.GQLWrapper (EncodeWrapperValue (..))
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    ArgumentsDefinition,
    CONST,
    Description,
    Directives,
    FieldName,
    IN,
    OUT,
    ObjectEntry (..),
    Position (..),
    TypeCategory (..),
    TypeName,
    TypeWrapper (..),
    Value (..),
    internal,
    mkBaseType,
    packName,
    toNullable,
    unitTypeName,
    unpackName,
  )
import Data.Sequence (Seq)
import Data.Text
  ( pack,
    unpack,
  )
import Data.Vector (Vector)
import GHC.Generics
import Relude hiding (Seq, Undefined, fromList, intercalate)

__typeData ::
  forall kinded (kind :: TypeCategory) (a :: Type).
  (GQLType a, CategoryValue kind) =>
  kinded kind a ->
  TypeData
__typeData proxy = __type proxy (categoryValue (Proxy @kind))

deriveTypename :: (GQLType a, CategoryValue kind) => kinded kind a -> TypeName
deriveTypename proxy = gqlTypeName $ __typeData proxy

deriveFingerprint :: (GQLType a, CategoryValue kind) => kinded kind a -> TypeFingerprint
deriveFingerprint proxy = gqlFingerprint $ __typeData proxy

deriveTypeData :: Typeable a => f a -> (Bool -> String -> String) -> TypeCategory -> TypeData
deriveTypeData proxy typeNameModifier cat =
  TypeData
    { gqlTypeName = packName . pack $ typeNameModifier (cat == IN) originalTypeName,
      gqlWrappers = mkBaseType,
      gqlFingerprint = getFingerprint cat proxy
    }
  where
    originalTypeName = unpack . unpackName $ getTypename proxy

list :: TypeWrapper -> TypeWrapper
list = flip TypeList True

wrapper :: (TypeWrapper -> TypeWrapper) -> TypeData -> TypeData
wrapper f TypeData {..} = TypeData {gqlWrappers = f gqlWrappers, ..}

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
{-# DEPRECATED getDirectives "use: directives" #-}

{-# DEPRECATED description "use: directive Describe { text } with typeDirective" #-}

{-# DEPRECATED getDescriptions "use: directive Describe { text } with fieldDirective" #-}

class GQLType a where
  type KIND a :: DerivingKind
  type KIND a = TYPE

  -- | A description of the type.
  --
  -- Used for documentation in the GraphQL schema.
  description :: f a -> Maybe Text
  description _ = Nothing

  directives :: f a -> DirectiveUsages
  directives _ = mempty

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
  __type proxy category = editTypeData derivedType (directives proxy)
    where
      derivedType = deriveTypeData proxy typeNameModifier category
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
instance GQLType () where
  __type _ = mkTypeData unitTypeName

instance GQLType a => GQLType (Maybe a) where
  type KIND (Maybe a) = WRAPPER
  __type _ = wrapper toNullable . __type (Proxy @a)

instance GQLType a => GQLType [a] where
  type KIND [a] = WRAPPER
  __type _ = wrapper list . __type (Proxy @a)

instance GQLType a => GQLType (Set a) where
  type KIND (Set a) = WRAPPER
  __type _ = __type $ Proxy @[a]

instance GQLType a => GQLType (NonEmpty a) where
  type KIND (NonEmpty a) = WRAPPER
  __type _ = __type $ Proxy @[a]

instance GQLType a => GQLType (Seq a) where
  type KIND (Seq a) = WRAPPER
  __type _ = __type $ Proxy @[a]

instance GQLType a => GQLType (Vector a) where
  type KIND (Vector a) = WRAPPER
  __type _ = __type $ Proxy @[a]

instance GQLType a => GQLType (SubscriptionField a) where
  type KIND (SubscriptionField a) = WRAPPER
  __type _ = __type $ Proxy @a

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b) where
  typeOptions _ = prefixInputs

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
  __type _ = __type $ Proxy @(Pair a b)
  typeOptions _ = prefixInputs

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

type Decode a = EncodeKind (KIND a) a

encodeArguments :: forall a. Decode a => a -> GQLResult (Arguments CONST)
encodeArguments x = encode x >>= unpackValue
  where
    unpackValue (Object v) = pure $ fmap toArgument v
    unpackValue _ = throwError (internal "TODO: expected arguments!")
    toArgument ObjectEntry {..} = Argument (Position 0 0) entryName entryValue

encode :: forall a. Decode a => a -> GQLResult (Value CONST)
encode x = encodeKind (ContextValue x :: ContextValue (KIND a) a)

class EncodeKind (kind :: DerivingKind) (a :: Type) where
  encodeKind :: ContextValue kind a -> GQLResult (Value CONST)

instance (EncodeWrapperValue f, Decode a) => EncodeKind WRAPPER (f a) where
  encodeKind = encodeWrapperValue encode . unContextValue

instance (EncodeScalar a) => EncodeKind SCALAR a where
  encodeKind = pure . Scalar . encodeScalar . unContextValue

instance (EncodeConstraint a) => EncodeKind TYPE a where
  encodeKind = exploreResolvers . unContextValue

convertNode ::
  DataType (GQLResult (Value CONST)) ->
  GQLResult (Value CONST)
convertNode
  DataType
    { tyIsUnion,
      tyCons = ConsRep {consFields, consName}
    } = encodeTypeFields consFields
    where
      encodeTypeFields ::
        [FieldRep (GQLResult (Value CONST))] -> GQLResult (Value CONST)
      encodeTypeFields [] = pure $ Enum consName
      encodeTypeFields fields | not tyIsUnion = Object <$> (traverse fromField fields >>= fromElems)
        where
          fromField FieldRep {fieldSelector, fieldValue} = do
            entryValue <- fieldValue
            pure ObjectEntry {entryName = fieldSelector, entryValue}
      -- Type References --------------------------------------------------------------
      encodeTypeFields _ = throwError (internal "TODO: union not supported")

-- Types & Constrains -------------------------------------------------------
class (EncodeKind (KIND a) a, GQLType a) => ExplorerConstraint a

instance (EncodeKind (KIND a) a, GQLType a) => ExplorerConstraint a

exploreResolvers :: forall a. EncodeConstraint a => a -> GQLResult (Value CONST)
exploreResolvers =
  convertNode
    . deriveValue
      ( DeriveValueOptions
          { __valueApply = encode,
            __valueTypeName = deriveTypename (KindedProxy :: KindedProxy IN a),
            __valueGQLOptions = typeOptions (Proxy @a) defaultTypeOptions,
            __valueGetType = __typeData . kinded (Proxy @IN)
          } ::
          DeriveValueOptions IN ExplorerConstraint (GQLResult (Value CONST))
      )

type EncodeConstraint a =
  ( Generic a,
    GQLType a,
    DeriveWith ExplorerConstraint (GQLResult (Value CONST)) (Rep a)
  )

class DeriveArguments (k :: DerivingKind) a where
  deriveArgumentsDefinition :: f k a -> SchemaT OUT (ArgumentsDefinition CONST)

-- DIRECTIVES

data DirectiveUsages = DirectiveUsages
  { typeDirectives :: [DirectiveUsage],
    fieldDirectives :: M.HashMap FieldName [DirectiveUsage],
    enumValueDirectives :: M.HashMap TypeName [DirectiveUsage]
  }

instance Monoid DirectiveUsages where
  mempty = DirectiveUsages mempty mempty mempty

instance Semigroup DirectiveUsages where
  DirectiveUsages td1 fd1 ed1 <> DirectiveUsages td2 fd2 ed2 = DirectiveUsages (td1 <> td2) (fd1 <> fd2) (ed1 <> ed2)

type TypeDirectiveConstraint a = (GQLDirective a, GQLType a, Decode a, DeriveArguments (KIND a) a, ToLocations (DIRECTIVE_LOCATIONS a))

typeDirective :: TypeDirectiveConstraint a => a -> DirectiveUsages
typeDirective x = DirectiveUsages [DirectiveUsage x] mempty mempty

fieldDirective :: TypeDirectiveConstraint a => FieldName -> a -> DirectiveUsages
fieldDirective fieldName x = DirectiveUsages mempty (M.singleton fieldName [DirectiveUsage x]) mempty

enumDirective :: TypeDirectiveConstraint a => TypeName -> a -> DirectiveUsages
enumDirective fieldName x = DirectiveUsages mempty mempty (M.singleton fieldName [DirectiveUsage x])

data DirectiveUsage where
  DirectiveUsage :: (GQLDirective a, GQLType a, Decode a, DeriveArguments (KIND a) a, ToLocations (DIRECTIVE_LOCATIONS a)) => a -> DirectiveUsage

applyOnTypeName :: DirectiveUsage -> TypeName -> TypeName
applyOnTypeName (DirectiveUsage x) = visitTypeName x

typeNameWithDirectives :: TypeName -> [DirectiveUsage] -> TypeName
typeNameWithDirectives = foldr applyOnTypeName

applyEnumDescription :: DirectiveUsage -> Maybe Description -> Maybe Description
applyEnumDescription (DirectiveUsage x) = visitEnumDescription x

applyFieldDescription :: DirectiveUsage -> Maybe Description -> Maybe Description
applyFieldDescription (DirectiveUsage x) = visitFieldDescription x

applyTypeDescription :: DirectiveUsage -> Maybe Description -> Maybe Description
applyTypeDescription (DirectiveUsage x) = visitTypeDescription x

editTypeData :: TypeData -> DirectiveUsages -> TypeData
editTypeData TypeData {..} DirectiveUsages {typeDirectives} = TypeData {gqlTypeName = typeNameWithDirectives gqlTypeName typeDirectives, ..}
