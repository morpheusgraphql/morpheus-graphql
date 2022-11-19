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

module Data.Morpheus.Server.Deriving.Named.EncodeValue
  ( EncodeFieldKind,
    Encode,
    encodeResolverValue,
    FieldConstraint,
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
import Data.Morpheus.Server.Deriving.Utils.Use (UseDeriving (..), UseGQLType (..))
import Data.Morpheus.Server.NamedResolvers
  ( NamedRef,
    NamedResolverT (..),
  )
import Data.Morpheus.Server.Types.GQLType (GQLType, GQLValue, KIND, withDir)
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

encodeResolverValue ::
  ( MonadError GQLError m,
    Generic a,
    gql [Maybe a],
    gql a,
    DeriveWith gql (Encode m) (m (ResolverValue m)) (Rep a)
  ) =>
  UseDeriving gql val ->
  [Maybe a] ->
  m [NamedResolverResult m]
encodeResolverValue drv x = traverse encodeNode x
  where
    encodeNode (Just v) = convertNamedNode drv (Identity x) (deriveValue (getOptions $ dirGQL drv) v)
    encodeNode Nothing = pure NamedNullResolver

type FieldConstraint gql m a =
  ( gql a,
    Generic a,
    DeriveWith gql (Encode m) (m (ResolverValue m)) (Rep a)
  )

class Encode (m :: Type -> Type) res where
  encodeField :: res -> m (ResolverValue m)

instance EncodeFieldKind GQLType GQLValue (KIND a) m a => Encode m a where
  encodeField resolver = encodeFieldKind withDir (ContextValue resolver :: ContextValue (KIND a) a)

class EncodeFieldKind gql val (k :: DerivingKind) (m :: Type -> Type) (a :: Type) where
  encodeFieldKind :: UseDeriving gql val -> ContextValue k a -> m (ResolverValue m)

instance (EncodeScalar a, Monad m) => EncodeFieldKind gql val SCALAR m a where
  encodeFieldKind _ = pure . ResScalar . encodeScalar . unContextValue

instance (MonadError GQLError m) => EncodeFieldKind gql val TYPE m a where
  encodeFieldKind _ (ContextValue _) = throwError (internal "types are resolved by Refs")

instance (Applicative m, EncodeFieldKind GQLType GQLValue (KIND a) m a) => EncodeFieldKind gql val WRAPPER m [a] where
  encodeFieldKind _ = fmap ResList . traverse encodeField . unContextValue

instance (gql a, EncodeFieldKind GQLType GQLValue (KIND a) m a, Applicative m) => EncodeFieldKind gql val WRAPPER m (Maybe a) where
  encodeFieldKind _ (ContextValue (Just x)) = encodeField x
  encodeFieldKind _ (ContextValue Nothing) = pure mkNull

instance (Monad m, gql a, ToJSON (NamedRef a)) => EncodeFieldKind gql val CUSTOM m (NamedResolverT m a) where
  encodeFieldKind drv = encodeRef . unContextValue
    where
      name :: TypeName
      name = useTypename (dirGQL drv) (OutputType :: CatType OUT a)
      encodeRef :: Monad m => NamedResolverT m a -> m (ResolverValue m)
      encodeRef (NamedResolverT ref) = do
        value <- replaceValue . toJSON <$> ref
        case value of
          (List ls) -> pure $ mkList $ map (packRef name) ls
          _ -> pure $ packRef name value

packRef :: Applicative m => TypeName -> ValidValue -> ResolverValue m
packRef name v = ResRef $ pure $ NamedResolverRef name [v]

instance
  ( Monad m,
    Encode (Resolver o e m) b,
    LiftOperation o,
    val a
  ) =>
  EncodeFieldKind gql val CUSTOM (Resolver o e m) (a -> b)
  where
  encodeFieldKind drv (ContextValue f) =
    getArguments
      >>= liftResolverState . useDecodeArguments drv
      >>= encodeField . f

getOptions :: UseGQLType gql -> DerivingOptions gql (Encode m) Identity (m (ResolverValue m))
getOptions UseGQLType {..} =
  DerivingOptions
    { optApply = encodeField . runIdentity,
      optTypeData = useTypeData . outputType
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
