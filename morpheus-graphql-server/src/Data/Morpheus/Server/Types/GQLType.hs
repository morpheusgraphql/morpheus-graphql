{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.GQLType
  ( GQLType (KIND, directives, __type),
    GQLValue (..),
    InputTypeNamespace (..),
    GQLResolver (..),
    ignoreUndefined,
    withGQL,
    withDir,
    withValue,
    withRes,
    kindedProxy,
    IgnoredResolver,
  )
where

-- MORPHEUS

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    ResolverState,
    ResolverValue,
    SubscriptionField,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (singleton)
import Data.Morpheus.Server.Deriving.Internal.Schema.Type
  ( fillTypeContent,
    injectType,
  )
import Data.Morpheus.Server.Deriving.Kinded.Arguments
  ( DeriveFieldArguments (..),
    HasArguments,
  )
import Data.Morpheus.Server.Deriving.Kinded.Resolver (KindedResolver (..))
import Data.Morpheus.Server.Deriving.Kinded.Type
  ( DERIVE_TYPE,
    DeriveKindedType (..),
    deriveInterfaceDefinition,
    deriveScalarDefinition,
    deriveTypeGuardUnions,
  )
import Data.Morpheus.Server.Deriving.Kinded.Value (KindedValue (..))
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatType (..), KindedProxy (KindedProxy), catMap, isIN)
import Data.Morpheus.Server.Deriving.Utils.Proxy (ContextValue (..), symbolName)
import Data.Morpheus.Server.Deriving.Utils.Use (UseDeriving (..), UseGQLType (..), UseResolver (..), UseValue (..))
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
import Data.Morpheus.Server.Types.NamedResolvers (NamedResolverT (..))
import Data.Morpheus.Server.Types.SchemaT (SchemaT, extendImplements)
import Data.Morpheus.Server.Types.TypeName
  ( TypeFingerprint (..),
    typeableFingerprint,
    typeableTypename,
  )
import Data.Morpheus.Server.Types.Types
  ( Arg,
    Pair,
    TypeGuard,
    Undefined (..),
    __typenameUndefined,
  )
import Data.Morpheus.Server.Types.Visitors (VisitType (..))
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    DirectiveLocation (..),
    FieldDefinition,
    GQLError,
    IN,
    Msg (msg),
    OUT,
    QUERY,
    ScalarDefinition (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    TypeWrapper (..),
    VALID,
    Value (..),
    internal,
    mkBaseType,
    mkField,
    toNullable,
    unitTypeName,
  )
import Data.Sequence (Seq)
import Data.Vector (Vector)
import GHC.Generics
import GHC.TypeLits (KnownSymbol)
import Relude hiding (Seq, Undefined, fromList, intercalate)

ignoreUndefined :: forall f a. GQLType a => f a -> Maybe (f a)
ignoreUndefined proxy
  | gqlFingerprint (__type (OutputType :: CatType OUT a)) == InternalFingerprint __typenameUndefined = Nothing
  | otherwise = Just proxy

deriveTypeData ::
  forall c (a :: Type).
  Typeable a =>
  CatType c a ->
  DirectiveUsages ->
  TypeData
deriveTypeData proxy GDirectiveUsages {typeDirectives} =
  TypeData
    { gqlTypeName = foldr (`applyTypeName` isIN proxy) (typeableTypename (Proxy @a)) typeDirectives,
      gqlWrappers = mkBaseType,
      gqlFingerprint = typeableFingerprint proxy
    }

list :: TypeWrapper -> TypeWrapper
list = flip TypeList True

wrapper :: (TypeWrapper -> TypeWrapper) -> TypeData -> TypeData
wrapper f TypeData {..} = TypeData {gqlWrappers = f gqlWrappers, ..}

type Lifted a = (PARAM (KIND a) a)

kindedProxy :: f a -> KindedProxy (KIND a) a
kindedProxy _ = KindedProxy

lifted :: CatType cat a -> CatType cat (f (KIND a) (Lifted a))
lifted InputType = InputType
lifted OutputType = OutputType

type IgnoredResolver = (Resolver QUERY () Identity)

-- lifts monadic object types with specific monad
type family PARAM k a where
  PARAM TYPE (t m) = t IgnoredResolver
  PARAM k a = a

type DERIVE_T c a = (DeriveKindedType GQLType GQLValue c (KIND a) (Lifted a))

cantBeInputType :: (MonadError GQLError m, GQLType a) => CatType cat a -> m b
cantBeInputType proxy = throwError $ internal $ "type " <> msg (gqlTypeName $ __type proxy) <> "can't be a input type"

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

  __type :: CatType cat a -> TypeData
  default __type :: Typeable a => CatType cat a -> TypeData
  __type proxy = deriveTypeData proxy (directives proxy)

  __deriveType :: CatType c a -> SchemaT c (TypeDefinition c CONST)
  default __deriveType :: DERIVE_T c a => CatType c a -> SchemaT c (TypeDefinition c CONST)
  __deriveType = deriveKindedType withDir . lifted

  __deriveFieldArguments :: CatType c a -> SchemaT c (Maybe (ArgumentsDefinition CONST))
  default __deriveFieldArguments ::
    DeriveFieldArguments GQLType (HasArguments a) =>
    CatType c a ->
    SchemaT c (Maybe (ArgumentsDefinition CONST))
  __deriveFieldArguments OutputType = deriveFieldArguments withDir (Proxy @(HasArguments a))
  __deriveFieldArguments InputType = pure Nothing

instance GQLType Int where
  type KIND Int = SCALAR
  __type = mkTypeData "Int"

instance GQLType Double where
  type KIND Double = SCALAR
  __type = mkTypeData "Float"

instance GQLType Float where
  type KIND Float = SCALAR
  __type = mkTypeData "Float32"

instance GQLType Text where
  type KIND Text = SCALAR
  __type = mkTypeData "String"

instance GQLType Bool where
  type KIND Bool = SCALAR
  __type = mkTypeData "Boolean"

instance GQLType ID where
  type KIND ID = SCALAR
  __type = mkTypeData "ID"

instance GQLType (Value CONST) where
  type KIND (Value CONST) = CUSTOM
  __type = mkTypeData "INTERNAL_VALUE"
  __deriveType = deriveScalarDefinition (const $ ScalarDefinition pure) withDir

-- WRAPPERS
instance GQLType () where
  __type = mkTypeData unitTypeName

instance Typeable m => GQLType (Undefined m) where
  __type = mkTypeData __typenameUndefined

instance GQLType a => GQLType (Maybe a) where
  type KIND (Maybe a) = WRAPPER
  __type = wrapper toNullable . __type . catMap (Proxy @a)

instance GQLType a => GQLType [a] where
  type KIND [a] = WRAPPER
  __type = wrapper list . __type . catMap (Proxy @a)

instance GQLType a => GQLType (Set a) where
  type KIND (Set a) = WRAPPER
  __type = __type . catMap (Proxy @[a])

instance GQLType a => GQLType (NonEmpty a) where
  type KIND (NonEmpty a) = WRAPPER
  __type = __type . catMap (Proxy @[a])

instance (GQLType a) => GQLType (Seq a) where
  type KIND (Seq a) = WRAPPER
  __type = __type . catMap (Proxy @[a])

instance GQLType a => GQLType (Vector a) where
  type KIND (Vector a) = WRAPPER
  __type = __type . catMap (Proxy @[a])

instance GQLType a => GQLType (SubscriptionField a) where
  type KIND (SubscriptionField a) = WRAPPER
  __type = __type . catMap (Proxy @a)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b) where
  directives _ = typeDirective InputTypeNamespace {inputTypeNamespace = "Input"}

-- Manual

instance (GQLType b, GQLType a) => GQLType (a -> b) where
  type KIND (a -> b) = CUSTOM
  __type = __type . catMap (Proxy @b)
  __deriveType OutputType = __deriveType (OutputType :: CatType OUT b)
  __deriveType proxy = cantBeInputType proxy

instance (GQLType k, GQLType v, Typeable k, Typeable v) => GQLType (Map k v) where
  type KIND (Map k v) = CUSTOM
  __type = __type . catMap (Proxy @[Pair k v])
  __deriveType = __deriveType . catMap (Proxy @[(k, v)])

instance GQLType a => GQLType (Resolver o e m a) where
  type KIND (Resolver o e m a) = CUSTOM
  __type = __type . catMap (Proxy @a)
  __deriveType = __deriveType . catMap (Proxy @a)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (a, b) where
  __type = __type . catMap (Proxy @(Pair a b))
  directives _ = typeDirective InputTypeNamespace {inputTypeNamespace = "Input"}

instance (KnownSymbol name, GQLType value) => GQLType (Arg name value) where
  type KIND (Arg name value) = CUSTOM
  __type = __type . catMap (Proxy @value)
  __deriveType OutputType = cantBeInputType (OutputType :: CatType OUT (Arg name value))
  __deriveType p@InputType = do
    injectType withDir proxy
    fillTypeContent withDir p content
    where
      content :: TypeContent TRUE IN CONST
      content = DataInputObject (singleton argName field)
      proxy = InputType :: CatType IN value
      argName = symbolName (Proxy @name)
      field :: FieldDefinition IN CONST
      field = mkField Nothing argName (TypeRef gqlTypeName gqlWrappers)
      TypeData {gqlTypeName, gqlWrappers} = useTypeData withGQL proxy

instance (DERIVE_TYPE GQLType OUT i, DERIVE_TYPE GQLType OUT u) => GQLType (TypeGuard i u) where
  type KIND (TypeGuard i u) = CUSTOM
  __type = __type . catMap (Proxy @i)
  __deriveType OutputType = do
    unions <- deriveTypeGuardUnions withDir union
    extendImplements (useTypename withGQL interface) unions
    deriveInterfaceDefinition withDir interface
    where
      interface = OutputType :: CatType OUT i
      union = OutputType :: CatType OUT u
  __deriveType proxy = cantBeInputType proxy

instance (GQLType a) => GQLType (NamedResolverT m a) where
  type KIND (NamedResolverT m a) = CUSTOM
  __type = __type . catMap (Proxy :: Proxy a)
  __deriveType = __deriveType . catMap (Proxy @a)
  __deriveFieldArguments = __deriveFieldArguments . catMap (Proxy @a)

type DirectiveUsages = GDirectiveUsages GQLType GQLValue

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

withValue :: UseValue GQLValue
withValue =
  UseValue
    { useDecodeValue = decodeValue,
      useEncodeValue = encodeValue
    }

withGQL :: UseGQLType GQLType
withGQL =
  UseGQLType
    { useFingerprint = gqlFingerprint . __type,
      useTypename = gqlTypeName . __type,
      useTypeData = __type,
      useDeriveType = __deriveType,
      useDeriveFieldArguments = __deriveFieldArguments
    }

withDir :: UseDeriving GQLType GQLValue
withDir =
  UseDeriving
    { __directives = directives,
      dirGQL = withGQL,
      dirArgs = withValue
    }

class GQLType a => GQLValue a where
  decodeValue :: Value VALID -> ResolverState a
  encodeValue :: a -> GQLResult (Value CONST)

instance (GQLType a, KindedValue GQLType GQLValue (KIND a) a) => GQLValue a where
  encodeValue value = encodeKindedValue withDir (ContextValue value :: ContextValue (KIND a) a)
  decodeValue = decodeKindedValue withDir (Proxy @(KIND a))

class GQLResolver (m :: Type -> Type) resolver where
  deriveResolver :: resolver -> m (ResolverValue m)

instance (KindedResolver GQLType GQLResolver GQLValue (KIND a) m a) => GQLResolver m a where
  deriveResolver resolver = kindedResolver withRes (ContextValue resolver :: ContextValue (KIND a) a)

withRes :: UseResolver GQLResolver GQLType GQLValue
withRes =
  UseResolver
    { useEncodeResolver = deriveResolver,
      resDrv = withDir
    }
