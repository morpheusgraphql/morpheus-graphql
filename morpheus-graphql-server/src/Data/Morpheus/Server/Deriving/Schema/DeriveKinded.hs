{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema.DeriveKinded
  ( DeriveKindedType (..),
    DeriveArgs (..),
    toFieldContent,
    DERIVE_TYPE,
  )
where

import Control.Monad.Except (throwError)
import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
  )
import Data.Morpheus.Internal.Ext
import Data.Morpheus.Internal.Utils (singleton)
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( CatType,
    TyContentM,
  )
import Data.Morpheus.Server.Deriving.Schema.TypeContent
import Data.Morpheus.Server.Deriving.Utils
  ( DeriveTypeOptions (..),
    DeriveWith,
    symbolName,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatContext (..),
    CatType (..),
    addContext,
    catMap,
    getCatContext,
    inputType,
    mkScalar,
    outputType,
    unliftKind,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
import Data.Morpheus.Server.Types.Internal (TypeData (..))
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    extendImplements,
  )
import Data.Morpheus.Server.Types.Types
  ( Arg (..),
    TypeGuard,
  )
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    scalarValidator,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    FieldContent (..),
    IN,
    OUT,
    ScalarDefinition (..),
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeName,
    TypeRef (..),
    UnionMember (memberName),
    Value,
    fieldsToArguments,
    mkField,
  )
import GHC.Generics
import GHC.TypeLits
import Relude

type DERIVE_TYPE gql k a = (gql a, DeriveWith gql gql (TyContentM k) (Rep a))

toFieldContent :: CatContext cat -> UseDirective gql dir -> UseGQLType derive -> DeriveTypeOptions cat gql derive (TyContentM cat)
toFieldContent ctx UseDirective {..} ops =
  DeriveTypeOptions
    { __typeGetType = __useTypeData dirGQL . addContext ctx,
      __typeApply = \proxy -> useDeriveType ops (addContext ctx proxy) *> useDeriveContent ops (addContext ctx proxy)
    }

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType gql dir (cat :: TypeCategory) (kind :: DerivingKind) a where
  deriveKindedType :: UseDirective gql dir -> UseGQLType gql -> CatType cat (f kind a) -> SchemaT cat ()
  deriveKindedContent :: UseDirective gql dir -> UseGQLType gql -> CatType cat (f kind a) -> TyContentM cat
  deriveKindedContent _ _ _ = pure Nothing

instance (gql a) => DeriveKindedType gql dir cat WRAPPER (f a) where
  deriveKindedType _ UseGQLType {..} = useDeriveType . catMap (Proxy @a)

instance (DecodeScalar a, gql a) => DeriveKindedType gql dir cat SCALAR a where
  deriveKindedType dirs _ proxy = insertTypeContent dirs (pure . mkScalar proxy . scalarValidator) (unliftKind proxy)

instance DERIVE_TYPE gql cat a => DeriveKindedType gql dir cat TYPE a where
  deriveKindedType dirs ops proxy = insertTypeContent dirs (deriveTypeContentWith dirs (toFieldContent (getCatContext proxy) dirs ops)) (unliftKind proxy)

instance (gql a) => DeriveKindedType gql dir cat CUSTOM (Resolver o e m a) where
  deriveKindedType _ UseGQLType {..} = useDeriveType . catMap (Proxy @a)

instance (gql (Value CONST)) => DeriveKindedType gql dir cat CUSTOM (Value CONST) where
  deriveKindedType dirs _ proxy = insertTypeContent dirs (const $ pure $ mkScalar proxy $ ScalarDefinition pure) (unliftKind proxy)

instance (gql [(k, v)]) => DeriveKindedType gql dir cat CUSTOM (Map k v) where
  deriveKindedType _ UseGQLType {..} = useDeriveType . catMap (Proxy @[(k, v)])

instance
  ( DERIVE_TYPE gql OUT interface,
    DERIVE_TYPE gql OUT union
  ) =>
  DeriveKindedType gql dir OUT CUSTOM (TypeGuard interface union)
  where
  deriveKindedType dir ops OutputType = do
    insertTypeContent dir (fmap DataInterface . deriveFieldsWith dir (toFieldContent OutputContext dir ops) . outputType) interfaceProxy
    content <- deriveTypeContentWith dir (toFieldContent OutputContext dir ops) (OutputType :: CatType OUT union)
    unionNames <- getUnionNames content
    extendImplements interfaceName unionNames
    where
      interfaceName :: TypeName
      interfaceName = __useTypename (dirGQL dir) interfaceProxy
      interfaceProxy :: CatType OUT interface
      interfaceProxy = OutputType
      unionProxy :: CatType OUT union
      unionProxy = OutputType
      getUnionNames :: TypeContent TRUE OUT CONST -> SchemaT OUT [TypeName]
      getUnionNames DataUnion {unionMembers} = pure $ toList $ memberName <$> unionMembers
      getUnionNames DataObject {} = pure [__useTypename (dirGQL dir) unionProxy]
      getUnionNames _ = throwError "guarded type must be an union or object"

instance (gql b, dir a) => DeriveKindedType gql dir OUT CUSTOM (a -> b) where
  deriveKindedContent dir UseGQLType {..} OutputType = do
    a <- useDeriveArguments (dirArgs dir) (Proxy @a)
    b <- useDeriveContent (OutputType :: CatType OUT b)
    case b of
      Just (FieldArgs x) -> Just . FieldArgs <$> (a <:> x)
      Nothing -> pure $ Just (FieldArgs a)
  deriveKindedType _ UseGQLType {..} OutputType = useDeriveType (outputType $ Proxy @b)

class DeriveArgs gql (k :: DerivingKind) a where
  deriveArgs :: UseDirective gql dir -> UseGQLType gql -> f k a -> SchemaT IN (ArgumentsDefinition CONST)

instance (DERIVE_TYPE gql IN a) => DeriveArgs gql TYPE a where
  deriveArgs dir ops = fmap fieldsToArguments . deriveFieldsWith dir (toFieldContent InputContext dir ops) . inputType

instance (KnownSymbol name, gql a) => DeriveArgs gql CUSTOM (Arg name a) where
  deriveArgs dir ops _ = do
    useDeriveType ops proxy
    pure $ fieldsToArguments $ singleton argName $ mkField Nothing argName argTypeRef
    where
      proxy :: CatType IN a
      proxy = InputType
      argName = symbolName (Proxy @name)
      argTypeRef = TypeRef {typeConName = gqlTypeName, typeWrappers = gqlWrappers}
      TypeData {gqlTypeName, gqlWrappers} = __useTypeData (dirGQL dir) proxy
