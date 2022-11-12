{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
  ( DeriveDirective,
    GQLType (KIND, directives, __type),
    InputTypeNamespace (..),
    deriveFingerprint,
    deriveTypename,
    encodeArguments,
    __isEmptyType,
    __typeData,
    withGQL,
    withDir,
    withDeriveType,
    DeriveType,
  )
where

-- MORPHEUS

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    SubscriptionField,
  )
import Data.Morpheus.Internal.Ext
import Data.Morpheus.Internal.Utils
import Data.Morpheus.Server.Deriving.Schema.DeriveKinded
import Data.Morpheus.Server.Deriving.Schema.Directive (UseDirective (..))
import Data.Morpheus.Server.Deriving.Schema.Internal
import Data.Morpheus.Server.Deriving.Utils (ConsRep (..), DataType (..), DeriveWith, FieldRep (..))
import Data.Morpheus.Server.Deriving.Utils.DeriveGType (DeriveValueOptions (..), deriveValue)
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (KindedProxy), inputType)
import Data.Morpheus.Server.Deriving.Utils.Proxy (ContextValue (..))
import Data.Morpheus.Server.Deriving.Utils.Use (UseArguments (..), UseDeriveType (..), UseGQLType (..))
import Data.Morpheus.Server.NamedResolvers (NamedResolverT (..))
import Data.Morpheus.Server.Types.Directives
  ( GDirectiveUsages (..),
    GQLDirective (..),
    applyTypeName,
    typeDirective,
  )
import Data.Morpheus.Server.Types.Internal
  ( TypeData (..),
    mkTypeData,
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.SchemaT (SchemaT, withInput)
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint (..), getFingerprint, getTypename)
import Data.Morpheus.Server.Types.Types
  ( Arg,
    Pair,
    TypeGuard,
    Undefined (..),
    __typenameUndefined,
  )
import Data.Morpheus.Server.Types.Visitors (VisitType (..))
import Data.Morpheus.Types.GQLScalar (EncodeScalar (..))
import Data.Morpheus.Types.GQLWrapper (EncodeWrapperValue (..))
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    ArgumentsDefinition,
    CONST,
    DirectiveLocation (..),
    GQLError,
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
    toNullable,
    unitTypeName,
  )
import Data.Sequence (Seq)
import Data.Vector (Vector)
import GHC.Generics
import GHC.TypeLits (KnownSymbol)
import Relude hiding (Seq, Undefined, fromList, intercalate)

__isEmptyType :: forall f a. GQLType a => f a -> Bool
__isEmptyType _ = deriveFingerprint (OutputType :: CatType OUT a) == InternalFingerprint __typenameUndefined

__typeData :: (GQLType a) => CatType cat a -> TypeData
__typeData proxy@InputType = __type proxy IN
__typeData proxy@OutputType = __type proxy OUT

deriveTypename :: (GQLType a) => CatType cat a -> TypeName
deriveTypename proxy = gqlTypeName $ __typeData proxy

deriveFingerprint :: (GQLType a) => CatType cat a -> TypeFingerprint
deriveFingerprint proxy = gqlFingerprint $ __typeData proxy

deriveTypeData ::
  Typeable a =>
  f a ->
  DirectiveUsages ->
  TypeCategory ->
  TypeData
deriveTypeData proxy GDirectiveUsages {typeDirectives} cat =
  TypeData
    { gqlTypeName = foldr (`applyTypeName` (cat == IN)) (getTypename proxy) typeDirectives,
      gqlWrappers = mkBaseType,
      gqlFingerprint = getFingerprint cat proxy
    }

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
--        directives _ = typeDirective (Describe "some text")
--  @
class GQLType a where
  type KIND a :: DerivingKind
  type KIND a = TYPE

  directives :: f a -> DirectiveUsages
  directives _ = mempty

  __type :: f a -> TypeCategory -> TypeData
  default __type :: Typeable a => f a -> TypeCategory -> TypeData
  __type proxy = deriveTypeData proxy (directives proxy)

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

instance GQLType (Value CONST) where
  type KIND (Value CONST) = CUSTOM
  __type _ = mkTypeData "INTERNAL_VALUE"

-- WRAPPERS
instance GQLType () where
  __type _ = mkTypeData unitTypeName

instance Typeable m => GQLType (Undefined m) where
  type KIND (Undefined m) = CUSTOM
  __type _ = mkTypeData __typenameUndefined

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
  directives _ = typeDirective InputTypeNamespace {inputTypeNamespace = "Input"}

-- Manual

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
  directives _ = typeDirective InputTypeNamespace {inputTypeNamespace = "Input"}

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

type EncodeValue a = EncodeKind (KIND a) a

encodeArguments :: forall m a. (MonadError GQLError m, EncodeValue a) => a -> m (Arguments CONST)
encodeArguments x = resultOr (const $ throwError err) pure (encode x) >>= unpackValue
  where
    err = internal "could not encode arguments. Arguments should be an object like type!"
    unpackValue (Object v) = pure $ fmap toArgument v
    unpackValue _ = throwError err
    toArgument ObjectEntry {..} = Argument (Position 0 0) entryName entryValue

encode :: forall a. EncodeValue a => a -> GQLResult (Value CONST)
encode x = encodeKind (ContextValue x :: ContextValue (KIND a) a)

class EncodeKind (kind :: DerivingKind) (a :: Type) where
  encodeKind :: ContextValue kind a -> GQLResult (Value CONST)

instance (EncodeWrapperValue f, EncodeValue a) => EncodeKind WRAPPER (f a) where
  encodeKind = encodeWrapperValue encode . unContextValue

instance (EncodeScalar a) => EncodeKind SCALAR a where
  encodeKind = pure . Scalar . encodeScalar . unContextValue

instance (EncodeConstraint a) => EncodeKind TYPE a where
  encodeKind = exploreResolvers . unContextValue

instance EncodeKind CUSTOM (Value CONST) where
  encodeKind = pure . unContextValue

-- TODO: remove me
instance (KnownSymbol name) => EncodeKind CUSTOM (Arg name a) where
  encodeKind _ = throwError "directives cant be tagged arguments"

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
      encodeTypeFields _ = throwError (internal "input unions are not supported")

-- Types & Constrains -------------------------------------------------------
class (EncodeKind (KIND a) a) => ExplorerConstraint a

instance (EncodeKind (KIND a) a) => ExplorerConstraint a

exploreResolvers :: forall a. EncodeConstraint a => a -> GQLResult (Value CONST)
exploreResolvers =
  convertNode
    . deriveValue
      ( DeriveValueOptions
          { __valueApply = encode,
            __valueTypeName = deriveTypename (InputType :: CatType IN a),
            __valueGetType = __typeData . inputType
          } ::
          DeriveValueOptions IN GQLType ExplorerConstraint (GQLResult (Value CONST))
      )

type EncodeConstraint a =
  ( Generic a,
    GQLType a,
    DeriveWith GQLType ExplorerConstraint (GQLResult (Value CONST)) (Rep a)
  )

-- DIRECTIVES
type DeriveArguments a = DeriveArgs GQLType DeriveType (KIND a) a

type DirectiveUsages = GDirectiveUsages GQLType DeriveDirective

deriveArguments :: DeriveArgs GQLType DeriveType k a => f k a -> SchemaT OUT (ArgumentsDefinition CONST)
deriveArguments = withInput . deriveArgs withDir withDeriveType

class (EncodeValue a, DeriveArguments a) => DeriveDirective a

instance (EncodeValue a, DeriveArguments a) => DeriveDirective a

newtype InputTypeNamespace = InputTypeNamespace {inputTypeNamespace :: Text}
  deriving (Generic)
  deriving anyclass
    (GQLType)

instance GQLDirective InputTypeNamespace where
  excludeFromSchema _ = True
  type
    DIRECTIVE_LOCATIONS InputTypeNamespace =
      '[ 'LOCATION_OBJECT,
         'LOCATION_ENUM,
         'LOCATION_INPUT_OBJECT,
         'LOCATION_UNION,
         'LOCATION_SCALAR,
         'LOCATION_INTERFACE
       ]

instance VisitType InputTypeNamespace where
  visitTypeName InputTypeNamespace {inputTypeNamespace} isInput name
    | isInput = inputTypeNamespace <> name
    | otherwise = name

withArgs :: UseArguments DeriveDirective
withArgs =
  UseArguments
    { useDeriveArguments = deriveArguments . withKind,
      useEncodeArguments = encodeArguments
    }

withGQL :: UseGQLType GQLType
withGQL =
  UseGQLType
    { __useFingerprint = \c v -> gqlFingerprint (__type v c),
      __useTypename = \c v -> gqlTypeName (__type v c),
      __useTypeData = __type
    }

withDir :: UseDirective GQLType DeriveDirective
withDir =
  UseDirective
    { __directives = directives,
      dirGQL = withGQL,
      dirArgs = withArgs
    }

withKind :: f a -> KindedProxy (KIND a) a
withKind _ = KindedProxy

withDeriveType :: UseDeriveType DeriveType
withDeriveType =
  UseDeriveType
    { useDeriveType = deriveType,
      useDeriveContent = deriveContent
    }

-- DERIVE TYPE

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class DeriveType (c :: TypeCategory) (a :: Type) where
  deriveType :: CatType c a -> SchemaT c ()
  deriveContent :: CatType c a -> TyContentM c

instance (GQLType a, DeriveKindedType GQLType DeriveType DeriveDirective cat (KIND a) a) => DeriveType cat a where
  deriveType = deriveKindedType withDir withDeriveType . liftKind
  deriveContent = deriveKindedContent withDir withDeriveType . liftKind

liftKind :: CatType cat a -> CatType cat (f (KIND a) a)
liftKind InputType = InputType
liftKind OutputType = OutputType
