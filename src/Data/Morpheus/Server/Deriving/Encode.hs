{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as M
  ( toList,
  )
import Data.Maybe
  ( Maybe (..),
    maybe,
  )
import Data.Morpheus.Kind
  ( GQL_KIND,
    INTERFACE,
    SCALAR,
    TYPE,
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
import Data.Morpheus.Types.GQLScalar (GQLScalar (..))
import Data.Morpheus.Types.Internal.AST
  ( InternalError,
    MUTATION,
    OperationType,
    QUERY,
    SUBSCRIPTION,
    TypeRef (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( FieldResModel,
    LiftOperation,
    ResModel (..),
    Resolver,
    ResolverState,
    RootResModel (..),
    SubscriptionField (..),
    failure,
    getArguments,
    liftResolverState,
    mkObject,
  )
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as S
  ( toList,
  )
import Data.Traversable (traverse)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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

class Encode o e (m :: * -> *) resolver where
  encode :: resolver -> Resolver o e m (ResModel o e m)

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) a o e m, LiftOperation o) => Encode o e m a where
  encode resolver = encodeKind (ContextValue resolver :: ContextValue (KIND a) a)

-- MAYBE
instance (Monad m, LiftOperation o, Encode o e m a) => Encode o e m (Maybe a) where
  encode = maybe (pure ResNull) encode

-- LIST []
instance (Monad m, Encode o e m a, LiftOperation o) => Encode o e m [a] where
  encode = fmap ResList . traverse encode

--  Tuple  (a,b)
instance Encode o e m (Pair k v) => Encode o e m (k, v) where
  encode (key, value) = encode (Pair key value)

--  NonEmpty
instance Encode o e m [a] => Encode o e m (NonEmpty a) where
  encode = encode . NonEmpty.toList

--  Vector
instance Encode o e m [a] => Encode o e m (Vector a) where
  encode = encode . Vector.toList

--  Set
instance Encode o e m [a] => Encode o e m (Set a) where
  encode = encode . S.toList

--  Map
instance (Monad m, LiftOperation o, Encode o e m (MapKind k v (Resolver o e m))) => Encode o e m (Map k v) where
  encode value =
    encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver o e m))

-- SUBSCRIPTION
instance (Monad m, LiftOperation o, Encode o e m a) => Encode o e m (SubscriptionField a) where
  encode (SubscriptionField _ res) = encode res

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance
  ( DecodeConstraint a,
    Generic a,
    Monad m,
    LiftOperation o,
    Encode o e m b
  ) =>
  Encode o e m (a -> b)
  where
  encode f =
    getArguments
      >>= liftResolverState . decodeArguments
      >>= encode . f

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (Monad m, Encode o e m b, LiftOperation o) => Encode o e m (Resolver o e m b) where
  encode x = x >>= encode

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o e (m :: * -> *) where
  encodeKind :: LiftOperation o => ContextValue kind a -> Resolver o e m (ResModel o e m)

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o e m where
  encodeKind = pure . ResScalar . serialize . unContextValue

instance EncodeConstraint o e m a => EncodeKind TYPE a o e m where
  encodeKind = pure . exploreResolvers . unContextValue

instance EncodeConstraint o e m a => EncodeKind INTERFACE a o e m where
  encodeKind = pure . exploreResolvers . unContextValue

convertNode ::
  (Monad m, LiftOperation o) =>
  DataType (Resolver o e m (ResModel o e m)) ->
  ResModel o e m
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
      encodeUnion [] = ResEnum tyName consName
      -- Type References --------------------------------------------------------------
      encodeUnion [FieldRep {fieldTypeRef = TypeRef {typeConName}, fieldValue}]
        | isUnionRef tyName cons = ResUnion typeConName fieldValue
      -- Inline Union Types ----------------------------------------------------------------------------
      encodeUnion fields =
        ResUnion
          consName
          $ pure
          $ mkObject
            consName
            (fmap toFieldRes fields)

-- Types & Constrains -------------------------------------------------------
exploreResolvers ::
  forall o e m a.
  ( EncodeConstraint o e m a,
    LiftOperation o
  ) =>
  a ->
  ResModel o e m
exploreResolvers =
  convertNode
    . toValue
      ( TypeConstraint (encode . runIdentity) ::
          TypeConstraint (Encode o e m) (Resolver o e m (ResModel o e m)) Identity
      )

----- HELPERS ----------------------------
objectResolvers ::
  ( EncodeConstraint o e m a,
    LiftOperation o
  ) =>
  a ->
  ResolverState (ResModel o e m)
objectResolvers value = constraintObject (exploreResolvers value)
  where
    constraintObject obj@ResObject {} =
      pure obj
    constraintObject _ =
      failure ("resolver must be an object" :: InternalError)

type EncodeObjectConstraint (o :: OperationType) e (m :: * -> *) a =
  EncodeConstraint o e m (a (Resolver o e m))

type EncodeConstraint (o :: OperationType) e (m :: * -> *) a =
  ( Monad m,
    GQLType a,
    Generic a,
    TypeRep (Encode o e m) (Resolver o e m (ResModel o e m)) (Rep a)
  )

type EncodeConstraints e m query mut sub =
  ( ChannelsConstraint e m sub,
    EncodeObjectConstraint QUERY e m query,
    EncodeObjectConstraint MUTATION e m mut,
    EncodeObjectConstraint SUBSCRIPTION e m sub
  )

toFieldRes :: FieldRep (Resolver o e m (ResModel o e m)) -> FieldResModel o e m
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
        | isEmptyType (Proxy :: Proxy (sub (Resolver SUBSCRIPTION e m))) = Nothing
        | otherwise = Just (channelResolver subscriptionResolver)
