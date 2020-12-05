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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Encode
  ( deriveModel,
    EncodeConstraints,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad (Monad ((>>=)))
import Data.Functor (fmap)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as M
  ( toList,
  )
import Data.Maybe
  ( Maybe (..),
  )
import Data.Morpheus.Kind
  ( GQL_KIND,
    INTERFACE,
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
    toValue,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (..))
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair (..),
    mapKindFromList,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper (EncodeWrapper (..))
import Data.Morpheus.Types.Internal.AST
  ( IN,
    InternalError,
    MUTATION,
    OperationType,
    QUERY,
    SUBSCRIPTION,
    TypeRef (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( LiftOperation,
    ResolvedObjectEntry,
    ResolvedValue (..),
    Resolver,
    ResolverState,
    RootResModel (..),
    failure,
    getArguments,
    liftResolverState,
    mkObject,
    mkUnion,
  )
import Data.Proxy (Proxy (..))
import GHC.Generics
  ( Generic (..),
  )
import Prelude
  ( ($),
    (.),
    otherwise,
  )

newtype ContextValue (kind :: GQL_KIND) a = ContextValue
  { unContextValue :: a
  }

class Encode (m :: * -> *) resolver where
  encode :: resolver -> m (ResolvedValue m)

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) m a) => Encode m a where
  encode resolver = encodeKind (ContextValue resolver :: ContextValue (KIND a) a)

--  Tuple  (a,b)
instance Encode m (Pair k v) => Encode m (k, v) where
  encode (key, value) = encode (Pair key value)

--  Map
instance (Monad m, Encode m (MapKind k v m)) => Encode m (Map k v) where
  encode value = encode ((mapKindFromList $ M.toList value) :: MapKind k v m)

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance
  ( DecodeConstraint a,
    Generic a,
    Monad m,
    Encode (Resolver o e m) b,
    LiftOperation o
  ) =>
  Encode (Resolver o e m) (a -> b)
  where
  encode f =
    getArguments
      >>= liftResolverState . decodeArguments
      >>= encode . f

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (Monad m, Encode (Resolver o e m) b, LiftOperation o) => Encode (Resolver o e m) (Resolver o e m b) where
  encode x = x >>= encode

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) (m :: * -> *) (a :: *) where
  encodeKind :: ContextValue kind a -> m (ResolvedValue m)

instance (EncodeWrapper f, Encode m a, Monad m) => EncodeKind WRAPPER m (f a) where
  encodeKind = encodeWrapper encode . unContextValue

instance (EncodeScalar a, Monad m) => EncodeKind SCALAR m a where
  encodeKind = pure . ResScalar . encodeScalar . unContextValue

instance EncodeConstraint m a => EncodeKind TYPE m a where
  encodeKind = pure . exploreResolvers . unContextValue

instance EncodeConstraint m a => EncodeKind INTERFACE m a where
  encodeKind = pure . exploreResolvers . unContextValue

convertNode ::
  Monad m =>
  DataType (m (ResolvedValue m)) ->
  ResolvedValue m
convertNode
  DataType
    { tyName,
      tyIsUnion,
      tyCons = cons@ConsRep {consFields, consName}
    }
    | tyIsUnion = encodeUnion consFields
    | otherwise = mkObject tyName (fmap toFieldRes consFields)
    where
      -- ENUM
      encodeUnion [] = ResEnum consName
      -- Type References --------------------------------------------------------------
      encodeUnion [FieldRep {fieldTypeRef = TypeRef {typeConName}, fieldValue}]
        | isUnionRef tyName cons = ResUnion typeConName fieldValue
      -- Inline Union Types ----------------------------------------------------------------------------
      encodeUnion fields = mkUnion consName (fmap toFieldRes fields)

-- Types & Constrains -------------------------------------------------------
exploreResolvers ::
  forall m a.
  ( EncodeConstraint m a
  ) =>
  a ->
  ResolvedValue m
exploreResolvers =
  convertNode
    . toValue
      ( TypeConstraint (encode . runIdentity) ::
          TypeConstraint (Encode m) (m (ResolvedValue m)) Identity
      )
      (Proxy @IN)

----- HELPERS ----------------------------
objectResolvers ::
  (EncodeConstraint m a) =>
  a ->
  ResolverState (ResolvedValue m)
objectResolvers value = constraintObject (exploreResolvers value)
  where
    constraintObject obj@ResObject {} =
      pure obj
    constraintObject _ =
      failure ("resolver must be an object" :: InternalError)

type EncodeConstraint (m :: * -> *) a =
  ( Monad m,
    GQLType a,
    Generic a,
    TypeRep (Encode m) (m (ResolvedValue m)) (Rep a)
  )

type EncodeObjectConstraint (o :: OperationType) e (m :: * -> *) a =
  EncodeConstraint (Resolver o e m) (a (Resolver o e m))

type EncodeConstraints e m query mut sub =
  ( ChannelsConstraint e m sub,
    EncodeObjectConstraint QUERY e m query,
    EncodeObjectConstraint MUTATION e m mut,
    EncodeObjectConstraint SUBSCRIPTION e m sub
  )

toFieldRes :: FieldRep (m (ResolvedValue m)) -> ResolvedObjectEntry m
toFieldRes FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)

deriveModel ::
  forall e m query mut sub.
  (Monad m, EncodeConstraints e m query mut sub) =>
  RootResolver m e query mut sub ->
  RootResModel e m
deriveModel
  RootResolver
    { queryResolver,
      mutationResolver,
      subscriptionResolver
    } =
    RootResModel
      { query = objectResolvers queryResolver,
        mutation = objectResolvers mutationResolver,
        subscription = objectResolvers subscriptionResolver,
        channelMap
      }
    where
      channelMap
        | __isEmptyType (Proxy :: Proxy (sub (Resolver SUBSCRIPTION e m))) = Nothing
        | otherwise = Just (channelResolver subscriptionResolver)
