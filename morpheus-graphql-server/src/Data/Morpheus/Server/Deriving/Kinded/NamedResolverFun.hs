{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.NamedResolverFun
  ( deriveNamedResolverFun,
    KindedNamedFunValue (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON (..))
import Data.Morpheus.App.Internal.Resolving
  ( MonadResolver (..),
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    ResolverValue (..),
    getArguments,
    mkList,
    mkNull,
  )
import Data.Morpheus.Generic
  ( GRep,
    GRepContext (..),
    GRepValue (..),
    deriveValue,
  )
import Data.Morpheus.Server.Deriving.Internal.Decode.Utils (useDecodeArguments)
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive (UseDeriving, toFieldRes)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    outputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseNamedResolver (..),
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.NamedResolvers
  ( NamedRef,
    NamedResolverT (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    OUT,
    TypeName,
    ValidValue,
    Value (List),
    internal,
    replaceValue,
  )
import GHC.Generics
  ( Generic (..),
  )
import Relude hiding (empty)

deriveNamedResolverFun ::
  ( Generic a,
    gql [Maybe a],
    gql a,
    MonadError GQLError m,
    GRep gql (res m) (m (ResolverValue m)) (Rep a)
  ) =>
  UseNamedResolver namedRes res gql val ->
  [Maybe a] ->
  m [NamedResolverResult m]
deriveNamedResolverFun ctx x = traverse encodeNode x
  where
    encodeNode (Just v) = convertNamedNode (namedDrv ctx) (Identity x) (deriveValue (getOptions ctx) v)
    encodeNode Nothing = pure NamedNullResolver

class KindedNamedFunValue ctx (k :: DerivingKind) (m :: Type -> Type) (a :: Type) where
  kindedNamedFunValue :: UseNamedResolver namedRes res gql val ~ ctx => ctx -> ContextValue k a -> m (ResolverValue m)

instance (EncodeScalar a, Monad m) => KindedNamedFunValue ctx SCALAR m a where
  kindedNamedFunValue _ = pure . ResScalar . encodeScalar . unContextValue

instance (MonadError GQLError m) => KindedNamedFunValue ctx TYPE m a where
  kindedNamedFunValue _ (ContextValue _) = throwError (internal "types are resolved by Refs")

instance (UseNamedResolver namedRes res gql val ~ ctx, Applicative m, res m a) => KindedNamedFunValue ctx WRAPPER m [a] where
  kindedNamedFunValue ctx = fmap ResList . traverse (useNamedFieldResolver ctx) . unContextValue

instance (UseNamedResolver namedRes res gql val ~ ctx, gql a, res m a, Applicative m) => KindedNamedFunValue ctx WRAPPER m (Maybe a) where
  kindedNamedFunValue ctx (ContextValue (Just x)) = useNamedFieldResolver ctx x
  kindedNamedFunValue _ (ContextValue Nothing) = pure mkNull

instance (UseNamedResolver namedRes res gql val ~ ctx, Monad m, gql a, ToJSON (NamedRef a)) => KindedNamedFunValue ctx CUSTOM m (NamedResolverT m a) where
  kindedNamedFunValue ctx = encodeRef . unContextValue
    where
      name :: TypeName
      name = useTypename (drvGQL (namedDrv ctx)) (OutputType :: CatType OUT a)
      encodeRef :: Monad m => NamedResolverT m a -> m (ResolverValue m)
      encodeRef (NamedResolverT ref) = do
        value <- replaceValue . toJSON <$> ref
        case value of
          (List ls) -> pure $ mkList $ map (packRef name) ls
          _ -> pure $ packRef name value

packRef :: Applicative m => TypeName -> ValidValue -> ResolverValue m
packRef name v = ResRef $ pure $ NamedResolverRef name [v]

instance (UseNamedResolver namedRes res gql val ~ ctx, Monad m, val a, MonadResolver m, res m b) => KindedNamedFunValue ctx CUSTOM m (a -> b) where
  kindedNamedFunValue ctx (ContextValue f) =
    getArguments
      >>= liftState . useDecodeArguments (namedDrv ctx)
      >>= useNamedFieldResolver ctx . f

getOptions :: UseNamedResolver namedRes res gql val -> GRepContext gql (res m) Identity (m (ResolverValue m))
getOptions UseNamedResolver {..} =
  GRepContext
    { optApply = useNamedFieldResolver . runIdentity,
      optTypeData = useTypeData (drvGQL namedDrv) . outputType
    }

convertNamedNode ::
  (gql a, MonadError GQLError m) =>
  UseDeriving gql val ->
  f a ->
  GRepValue (m (ResolverValue m)) ->
  m (NamedResolverResult m)
convertNamedNode _ _ GRepValueEnum {..} = pure $ NamedEnumResolver enumVariantName
convertNamedNode drv proxy GRepValueObject {..} = pure $ NamedObjectResolver $ ObjectTypeResolver $ fromList (toFieldRes drv proxy <$> objectFields)
convertNamedNode _ _ GRepValueUnionRef {..} = NamedUnionResolver <$> (unionRefValue >>= getRef)
convertNamedNode _ _ GRepValueUnion {} = throwError "only union references are supported!"

getRef :: MonadError GQLError m => ResolverValue m -> m NamedResolverRef
getRef (ResRef x) = x
getRef _ = throwError "only resolver references are supported!"
