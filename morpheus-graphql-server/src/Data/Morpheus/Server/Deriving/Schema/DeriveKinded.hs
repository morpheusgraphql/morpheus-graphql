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
    getCat,
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
    TypeCategory (IN),
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

type DERIVE_TYPE gql derive k a = (gql a, DeriveWith gql derive (TyContentM k) (Rep a))

toFieldContent :: CatContext kind -> UseDirective gql dir -> UseDeriveType derive -> DeriveTypeOptions kind gql derive (TyContentM kind)
toFieldContent catCtx UseDirective {..} ops =
  DeriveTypeOptions
    { __typeGetType = \x -> __useTypeData dirGQL x (getCat catCtx),
      __typeApply = \proxy -> useDeriveType ops (addContext catCtx proxy) *> useDeriveContent ops (addContext catCtx proxy)
    }

-- | DeriveType With specific Kind: 'kind': object, scalar, enum ...
class DeriveKindedType gql derive dir (cat :: TypeCategory) (kind :: DerivingKind) a where
  deriveKindedType :: UseDirective gql dir -> UseDeriveType derive -> CatType cat (f kind a) -> SchemaT cat ()
  deriveKindedContent :: UseDirective gql dir -> UseDeriveType derive -> CatType cat (f kind a) -> TyContentM cat
  deriveKindedContent _ _ _ = pure Nothing

instance (derive a) => DeriveKindedType gql derive dir cat WRAPPER (f a) where
  deriveKindedType _ UseDeriveType {..} = useDeriveType . catMap (Proxy @a)

instance (DecodeScalar a, gql a) => DeriveKindedType gql derive dir cat SCALAR a where
  deriveKindedType dirs _ proxy = insertTypeContent dirs (pure . mkScalar proxy . scalarValidator) (unliftKind proxy)

instance DERIVE_TYPE gql derive cat a => DeriveKindedType gql derive dir cat TYPE a where
  deriveKindedType dirs ops proxy = insertTypeContent dirs (deriveTypeContentWith dirs (toFieldContent (getCatContext proxy) dirs ops)) (unliftKind proxy)

instance (derive a) => DeriveKindedType gql derive dir cat CUSTOM (Resolver o e m a) where
  deriveKindedType _ UseDeriveType {..} = useDeriveType . catMap (Proxy @a)

instance (gql (Value CONST)) => DeriveKindedType gql derive dir cat CUSTOM (Value CONST) where
  deriveKindedType dirs _ proxy = insertTypeContent dirs (const $ pure $ mkScalar proxy $ ScalarDefinition pure) (unliftKind proxy)

instance (derive [(k, v)]) => DeriveKindedType gql derive dir cat CUSTOM (Map k v) where
  deriveKindedType _ UseDeriveType {..} = useDeriveType . catMap (Proxy @[(k, v)])

instance
  ( DERIVE_TYPE gql derive cat interface,
    DERIVE_TYPE gql derive cat union
  ) =>
  DeriveKindedType gql derive dir cat CUSTOM (TypeGuard interface union)
  where
  deriveKindedType dir ops OutputType = do
    insertTypeContent dir (fmap DataInterface . deriveFieldsWith dir (toFieldContent OutputContext dir ops) . outputType) interfaceProxy
    content <- deriveTypeContentWith dir (toFieldContent OutputContext dir ops) (OutputType :: CatType OUT union)
    unionNames <- getUnionNames content
    extendImplements interfaceName unionNames
    where
      interfaceName :: TypeName
      interfaceName = useTypename (dirGQL dir) interfaceProxy
      interfaceProxy :: CatType OUT interface
      interfaceProxy = OutputType
      unionProxy :: CatType OUT union
      unionProxy = OutputType
      getUnionNames :: TypeContent TRUE OUT CONST -> SchemaT OUT [TypeName]
      getUnionNames DataUnion {unionMembers} = pure $ toList $ memberName <$> unionMembers
      getUnionNames DataObject {} = pure [useTypename (dirGQL dir) unionProxy]
      getUnionNames _ = throwError "guarded type must be an union or object"

instance (derive b, dir a) => DeriveKindedType gql derive dir cat CUSTOM (a -> b) where
  deriveKindedContent dir UseDeriveType {..} OutputType = do
    a <- useDeriveArguments (dirArgs dir) (Proxy @a)
    b <- useDeriveContent (OutputType :: CatType OUT b)
    case b of
      Just (FieldArgs x) -> Just . FieldArgs <$> (a <:> x)
      Nothing -> pure $ Just (FieldArgs a)
  deriveKindedType _ UseDeriveType {..} OutputType = useDeriveType (outputType $ Proxy @b)

class DeriveArgs gql derive (k :: DerivingKind) a where
  deriveArgs :: UseDirective gql dir -> UseDeriveType derive -> f k a -> SchemaT IN (ArgumentsDefinition CONST)

instance (DERIVE_TYPE gql derive IN a) => DeriveArgs gql derive TYPE a where
  deriveArgs dir ops = fmap fieldsToArguments . deriveFieldsWith dir (toFieldContent InputContext dir ops) . inputType

instance (KnownSymbol name, derive a, gql a) => DeriveArgs gql derive CUSTOM (Arg name a) where
  deriveArgs dir ops _ = do
    useDeriveType ops proxy
    pure $ fieldsToArguments $ singleton argName $ mkField Nothing argName argTypeRef
    where
      proxy :: CatType IN a
      proxy = InputType
      argName = symbolName (Proxy @name)
      argTypeRef = TypeRef {typeConName = gqlTypeName, typeWrappers = gqlWrappers}
      TypeData {gqlTypeName, gqlWrappers} = __useTypeData (dirGQL dir) proxy IN
