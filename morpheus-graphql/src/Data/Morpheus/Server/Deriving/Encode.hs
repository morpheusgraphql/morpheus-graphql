{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Encode
  ( deriveModel,
    EncodeConstraints,
    ContextValue (..),
  )
where

import Control.Monad.Except (MonadError)
import qualified Data.Map as M
import Data.Morpheus.App.Internal.Resolving
  ( LiftOperation,
    ObjectTypeResolver,
    Resolver,
    ResolverState,
    ResolverValue (..),
    RootResolverValue (..),
    getArguments,
    liftResolverState,
    mkEnum,
    mkObject,
    mkUnion,
    requireObject,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Deriving.Channels
  ( ChannelsConstraint,
    channelResolver,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeConstraint,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DataType (..),
    FieldRep (..),
    TypeConstraint (..),
    TypeRep (..),
    isUnionRef,
    toFieldRes,
    toValue,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType,
    KIND,
    __isEmptyType,
  )
import Data.Morpheus.Server.Types.Types
  ( TypeGuard (..),
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper (EncodeWrapper (..))
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    IN,
    MUTATION,
    OperationType,
    QUERY,
    SUBSCRIPTION,
    TypeRef (..),
  )
import GHC.Generics
  ( Generic (..),
  )
import Relude

newtype ContextValue (kind :: DerivingKind) a = ContextValue
  { unContextValue :: a
  }

class Encode (m :: Type -> Type) resolver where
  encode :: resolver -> m (ResolverValue m)

instance (EncodeKind (KIND a) m a) => Encode m a where
  encode resolver = encodeKind (ContextValue resolver :: ContextValue (KIND a) a)

-- ENCODE GQL KIND
class EncodeKind (kind :: DerivingKind) (m :: Type -> Type) (a :: Type) where
  encodeKind :: ContextValue kind a -> m (ResolverValue m)

instance
  ( EncodeWrapper f,
    Encode m a,
    Monad m
  ) =>
  EncodeKind WRAPPER m (f a)
  where
  encodeKind = encodeWrapper encode . unContextValue

instance
  ( EncodeScalar a,
    Monad m
  ) =>
  EncodeKind SCALAR m a
  where
  encodeKind = pure . ResScalar . encodeScalar . unContextValue

instance
  ( EncodeConstraint m a,
    MonadError GQLError m
  ) =>
  EncodeKind TYPE m a
  where
  encodeKind = pure . exploreResolvers . unContextValue

--  Map
instance (Monad m, Encode m [(k, v)]) => EncodeKind CUSTOM m (Map k v) where
  encodeKind = encode . M.toList . unContextValue

--  INTERFACE Types
instance (MonadError GQLError m, EncodeConstraint m guard, EncodeConstraint m union) => EncodeKind CUSTOM m (TypeGuard guard union) where
  encodeKind (ContextValue (ResolveType value)) = pure (exploreResolvers value)
  encodeKind (ContextValue (ResolveInterface value)) = pure (exploreResolvers value)

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance
  ( DecodeConstraint a,
    Generic a,
    Monad m,
    Encode (Resolver o e m) b,
    LiftOperation o
  ) =>
  EncodeKind CUSTOM (Resolver o e m) (a -> b)
  where
  encodeKind (ContextValue f) =
    getArguments
      >>= liftResolverState . decodeArguments
      >>= encode . f

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance
  (Monad m, Encode (Resolver o e m) b, LiftOperation o) =>
  EncodeKind CUSTOM (Resolver o e m) (Resolver o e m b)
  where
  encodeKind (ContextValue value) = value >>= encode

convertNode ::
  forall m.
  MonadError GQLError m =>
  DataType (m (ResolverValue m)) ->
  ResolverValue m
convertNode
  DataType
    { dataTypeName,
      tyIsUnion,
      tyCons = cons@ConsRep {consFields, consName}
    } = encodeTypeFields consFields
    where
      -- ENUM
      encodeTypeFields ::
        [FieldRep (m (ResolverValue m))] ->
        ResolverValue m
      encodeTypeFields [] = mkEnum consName
      encodeTypeFields fields
        | not tyIsUnion = mkObject dataTypeName (toFieldRes <$> fields)
      -- Type References --------------------------------------------------------------
      encodeTypeFields [FieldRep {fieldTypeRef, fieldValue}]
        | isUnionRef dataTypeName cons = ResLazy (ResObject (Just (typeConName fieldTypeRef)) <$> (fieldValue >>= requireObject))
      -- Inline Union Types ----------------------------------------------------------------------------
      encodeTypeFields fields = mkUnion consName (toFieldRes <$> fields)

-- Types & Constrains -------------------------------------------------------
exploreResolvers ::
  forall m a.
  ( EncodeConstraint m a,
    MonadError GQLError m
  ) =>
  a ->
  ResolverValue m
exploreResolvers =
  convertNode
    . toValue
      ( TypeConstraint (encode . runIdentity) ::
          TypeConstraint (Encode m) (m (ResolverValue m)) Identity
      )
      (Proxy @IN)

----- HELPERS ----------------------------
objectResolvers ::
  ( EncodeConstraint m a,
    MonadError GQLError m
  ) =>
  a ->
  ResolverState (ObjectTypeResolver m)
objectResolvers value = requireObject (exploreResolvers value)

type EncodeConstraint (m :: Type -> Type) a =
  ( GQLType a,
    Generic a,
    TypeRep (Encode m) (m (ResolverValue m)) (Rep a)
  )

type EncodeObjectConstraint (o :: OperationType) e (m :: Type -> Type) a =
  EncodeConstraint (Resolver o e m) (a (Resolver o e m))

type EncodeConstraints e m query mut sub =
  ( ChannelsConstraint e m sub,
    EncodeObjectConstraint QUERY e m query,
    EncodeObjectConstraint MUTATION e m mut,
    EncodeObjectConstraint SUBSCRIPTION e m sub
  )

deriveModel ::
  forall e m query mut sub.
  (Monad m, EncodeConstraints e m query mut sub) =>
  RootResolver m e query mut sub ->
  GQLResult (RootResolverValue e m)
deriveModel RootResolver {..} =
  pure
    RootResolverValue
      { queryResolver = objectResolvers queryResolver,
        mutationResolver = objectResolvers mutationResolver,
        subscriptionResolver = objectResolvers subscriptionResolver,
        channelMap
      }
  where
    channelMap
      | __isEmptyType (Proxy :: Proxy (sub (Resolver SUBSCRIPTION e m))) = Nothing
      | otherwise = Just (channelResolver subscriptionResolver)
