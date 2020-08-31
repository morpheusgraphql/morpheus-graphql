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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
    maybe,
  )
import Data.Morpheus.Internal.Utils (Namespace (..))
import Data.Morpheus.Kind
  ( ENUM,
    GQL_KIND,
    INTERFACE,
    OUTPUT,
    SCALAR,
  )
import Data.Morpheus.Server.Deriving.Channels
  ( ChannelsConstraint,
    getChannels,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeType,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConRep (..),
    ConsRep (..),
    DataType (..),
    FieldRep (..),
    TypeConstraint (..),
    conNameProxy,
    datatypeNameProxy,
    deriveFieldRep,
    enumerate,
    isRecordProxy,
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
    TypeName,
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
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
  ( toList,
  )
import Data.Traversable (traverse)
import GHC.Generics
  ( (:*:) (..),
    (:+:) (..),
    C,
    Constructor,
    D,
    Datatype,
    Generic (..),
    K1 (..),
    M1 (..),
    Rec0,
    S,
    Selector,
    U1 (..),
  )
import Prelude
  ( ($),
    (&&),
    (.),
    Bool (..),
    Eq (..),
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
  ( DecodeType a,
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

type EncodeConstraint o e m a =
  ( Monad m,
    Generic a,
    GQLType a,
    TypeRep (Rep a) (Resolver o e m (ResModel o e m))
  )

-- ENUM
instance EncodeConstraint o e m a => EncodeKind ENUM a o e m where
  encodeKind = liftResolverState . exploreResolvers . unContextValue

instance EncodeConstraint o e m a => EncodeKind OUTPUT a o e m where
  encodeKind = liftResolverState . exploreResolvers . unContextValue

instance EncodeConstraint o e m a => EncodeKind INTERFACE a o e m where
  encodeKind = liftResolverState . exploreResolvers . unContextValue

convertNode ::
  (Monad m, LiftOperation o) =>
  Maybe TypeName ->
  DataType (Resolver o e m (ResModel o e m)) ->
  ResModel o e m
convertNode _ DataType {tyName, tyIsUnion = False, tyCons = ConsRep {consFields}} =
  mkObject tyName (fmap toFieldRes consFields)
convertNode
  namespace
  DataType
    { tyName,
      tyIsUnion = True,
      tyCons = ConsRep {consFields, consIsRecord, consName}
    } =
    encodeUnion consFields
    where
      -- ENUM
      encodeUnion [] = ResEnum tyName (stripNamespace namespace consName)
      -- Type References --------------------------------------------------------------
      encodeUnion [FieldRep {fieldTypeRef = TypeRef {typeConName}, fieldValue, fieldIsObject}]
        | fieldIsObject && consName == tyName <> typeConName =
          ResUnion typeConName fieldValue
      -- Inline Union Types ----------------------------------------------------------------------------
      encodeUnion fields =
        ResUnion
          consName
          $ pure
          $ mkObject
            consName
            (fmap toFieldRes resolvers)
        where
          resolvers
            | consIsRecord = fields
            | otherwise = enumerate fields

-- Types & Constrains -------------------------------------------------------

exploreResolvers ::
  forall o e m a.
  ( EncodeConstraint o e m a,
    LiftOperation o
  ) =>
  a ->
  ResolverState (ResModel o e m)
exploreResolvers =
  pure
    . convertNode (getNamespace (Proxy @a))
    . stripNamespace (getNamespace (Proxy @a))
    . typeResolvers
    . from

----- HELPERS ----------------------------
objectResolvers ::
  forall a o e m.
  ( Monad m,
    LiftOperation o,
    GQLType a,
    Generic a,
    TypeRep (Rep a) (Resolver o e m (ResModel o e m))
  ) =>
  a ->
  ResolverState (ResModel o e m)
objectResolvers value =
  exploreResolvers value
    >>= constraintObject
  where
    constraintObject obj@ResObject {} =
      pure obj
    constraintObject _ =
      failure ("resolver must be an object" :: InternalError)

type EncodeObjectConstraint (o :: OperationType) e (m :: * -> *) a =
  TypeConst o e m (a (Resolver o e m))

type TypeConst (o :: OperationType) e m a =
  ( GQLType a,
    Generic a,
    TypeRep (Rep a) (Resolver o e m (ResModel o e m))
  )

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
        | otherwise = Just (getChannels subscriptionResolver)

toFieldRes :: FieldRep (Resolver o e m (ResModel o e m)) -> FieldResModel o e m
toFieldRes FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)

class TypeRep f (v :: *) where
  typeResolvers :: f a -> DataType v

instance (Datatype d, TypeRep f v) => TypeRep (M1 D d f) v where
  typeResolvers (M1 src) = (typeResolvers src) {tyName = datatypeNameProxy (Proxy @d)}

--- UNION OR OBJECT
instance (TypeRep a v, TypeRep b v) => TypeRep (a :+: b) v where
  typeResolvers (L1 x) = (typeResolvers x) {tyIsUnion = True}
  typeResolvers (R1 x) = (typeResolvers x) {tyIsUnion = True}

instance
  ( Constructor c,
    ConRep (Encode o e m) (Resolver o e m (ResModel o e m)) f
  ) =>
  TypeRep (M1 C c f) (Resolver o e m (ResModel o e m))
  where
  typeResolvers (M1 src) =
    DataType
      { tyName = "",
        tyIsUnion = False,
        tyCons =
          ConsRep
            { consName = conNameProxy (Proxy @c),
              consIsRecord = isRecordProxy (Proxy @c),
              consFields =
                toFieldRep
                  ( TypeConstraint (encode . runIdentity) ::
                      TypeConstraint (Encode o e m) (Resolver o e m (ResModel o e m)) Identity
                  )
                  src
            }
      }
