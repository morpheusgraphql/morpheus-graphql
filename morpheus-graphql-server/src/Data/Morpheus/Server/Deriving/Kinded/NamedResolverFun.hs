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
  ( LiftOperation,
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    Resolver,
    ResolverValue (..),
    getArguments,
    liftResolverState,
    mkList,
    mkNull,
  )
import Data.Morpheus.Server.Deriving.Internal.Decode.Utils (useDecodeArguments)
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive (UseDeriving, toFieldRes)
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveWith,
    DerivingOptions (..),
    deriveValue,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    outputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Types
  ( ConsRep (..),
    DataType (..),
    FieldRep (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Use (UseDeriving (..), UseGQLType (..), UseNamedResolver (..))
import Data.Morpheus.Server.NamedResolvers
  ( NamedRef,
    NamedResolverT (..),
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
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
import qualified GHC.Exts as HM
import GHC.Generics
  ( Generic (..),
  )
import Relude hiding (empty)

deriveNamedResolverFun ::
  ( Generic a,
    gql [Maybe a],
    gql a,
    MonadError GQLError m,
    DeriveWith gql (res m) (m (ResolverValue m)) (Rep a)
  ) =>
  UseNamedResolver res gql val ->
  [Maybe a] ->
  m [NamedResolverResult m]
deriveNamedResolverFun ctx x = traverse encodeNode x
  where
    encodeNode (Just v) = convertNamedNode (namedDrv ctx) (Identity x) (deriveValue (getOptions ctx) v)
    encodeNode Nothing = pure NamedNullResolver

class KindedNamedFunValue res gql val (k :: DerivingKind) (m :: Type -> Type) (a :: Type) where
  kindedNamedFunValue :: UseNamedResolver res gql val -> ContextValue k a -> m (ResolverValue m)

instance (EncodeScalar a, Monad m) => KindedNamedFunValue res gql val SCALAR m a where
  kindedNamedFunValue _ = pure . ResScalar . encodeScalar . unContextValue

instance (MonadError GQLError m) => KindedNamedFunValue res gql val TYPE m a where
  kindedNamedFunValue _ (ContextValue _) = throwError (internal "types are resolved by Refs")

instance (Applicative m, res m a) => KindedNamedFunValue res gql val WRAPPER m [a] where
  kindedNamedFunValue ctx = fmap ResList . traverse (useNamedFieldResolver ctx) . unContextValue

instance (gql a, res m a, Applicative m) => KindedNamedFunValue res gql val WRAPPER m (Maybe a) where
  kindedNamedFunValue ctx (ContextValue (Just x)) = useNamedFieldResolver ctx x
  kindedNamedFunValue _ (ContextValue Nothing) = pure mkNull

instance (Monad m, gql a, ToJSON (NamedRef a)) => KindedNamedFunValue res gql val CUSTOM m (NamedResolverT m a) where
  kindedNamedFunValue ctx = encodeRef . unContextValue
    where
      name :: TypeName
      name = useTypename (dirGQL (namedDrv ctx)) (OutputType :: CatType OUT a)
      encodeRef :: Monad m => NamedResolverT m a -> m (ResolverValue m)
      encodeRef (NamedResolverT ref) = do
        value <- replaceValue . toJSON <$> ref
        case value of
          (List ls) -> pure $ mkList $ map (packRef name) ls
          _ -> pure $ packRef name value

packRef :: Applicative m => TypeName -> ValidValue -> ResolverValue m
packRef name v = ResRef $ pure $ NamedResolverRef name [v]

instance (Monad m, LiftOperation o, val a, res (Resolver o e m) b) => KindedNamedFunValue res gql val CUSTOM (Resolver o e m) (a -> b) where
  kindedNamedFunValue ctx (ContextValue f) =
    getArguments
      >>= liftResolverState . useDecodeArguments (namedDrv ctx)
      >>= useNamedFieldResolver ctx . f

getOptions :: UseNamedResolver res gql val -> DerivingOptions gql (res m) Identity (m (ResolverValue m))
getOptions UseNamedResolver {..} =
  DerivingOptions
    { optApply = useNamedFieldResolver . runIdentity,
      optTypeData = useTypeData (dirGQL namedDrv) . outputType
    }

convertNamedNode ::
  (gql a, MonadError GQLError m) =>
  UseDeriving gql val ->
  f a ->
  DataType (m (ResolverValue m)) ->
  m (NamedResolverResult m)
convertNamedNode
  drv
  proxy
  DataType
    { tyIsUnion,
      tyCons = ConsRep {consFields, consName}
    }
    | null consFields = pure $ NamedEnumResolver consName
    | tyIsUnion = deriveUnion consFields
    | otherwise =
        pure $
          NamedObjectResolver
            ObjectTypeResolver
              { objectFields = HM.fromList (toFieldRes drv proxy <$> consFields)
              }

deriveUnion :: (MonadError GQLError m) => [FieldRep (m (ResolverValue m))] -> m (NamedResolverResult m)
deriveUnion [FieldRep {..}] = NamedUnionResolver <$> (fieldValue >>= getRef)
deriveUnion _ = throwError "only union references are supported!"

getRef :: MonadError GQLError m => ResolverValue m -> m NamedResolverRef
getRef (ResRef x) = x
getRef _ = throwError "only resolver references are supported!"
