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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Encode
  ( deriveModel,
    deriveNamedModel,
    EncodeConstraints,
    EncodeNamedConstraints,
  )
where

import Control.Monad.Except (MonadError)
import qualified Data.Map as M
import Data.Morpheus.App.Internal.Resolving
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.NamedResolvers (RefResolver, ResolveNamed (Dep, resolveNamed))
import Data.Morpheus.Server.Deriving.Channels
  ( ChannelsConstraint,
    channelResolver,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( Decode (decode),
    DecodeConstraint,
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
import Data.Morpheus.Server.Deriving.Utils.GTraversble
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (KindedProxy))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__type),
    KIND,
    TypeData (gqlTypeName),
    __isEmptyType,
  )
import Data.Morpheus.Server.Types.Types
  ( Pair (..),
    TypeGuard (..),
  )
import Data.Morpheus.Types
  ( NamedResolvers (..),
    RootResolver (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper (EncodeWrapper (..))
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    IN,
    MUTATION,
    OUT,
    OperationType,
    QUERY,
    SUBSCRIPTION,
    TypeCategory (OUT),
    TypeName,
    TypeRef (..),
    ValidValue,
    Value (Null),
  )
import qualified GHC.Exts as HM
import GHC.Generics
  ( Generic (..),
  )
import Relude

newtype ContextValue (kind :: DerivingKind) a = ContextValue
  { unContextValue :: a
  }

class Encode (m :: * -> *) resolver where
  encode :: resolver -> m (ResolverValue m)

instance (EncodeKind (KIND a) m a) => Encode m a where
  encode resolver = encodeKind (ContextValue resolver :: ContextValue (KIND a) a)

-- ENCODE GQL KIND
class EncodeKind (kind :: DerivingKind) (m :: * -> *) (a :: *) where
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

--  Tuple  (a,b)
instance
  Encode m (Pair k v) =>
  EncodeKind CUSTOM m (k, v)
  where
  encodeKind = encode . uncurry Pair . unContextValue

--  Map
instance (Monad m, Encode m [Pair k v]) => EncodeKind CUSTOM m (Map k v) where
  encodeKind = encode . fmap (uncurry Pair) . M.toList . unContextValue

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
    { tyName,
      tyIsUnion,
      tyCons = cons@ConsRep {consFields, consName}
    }
    | tyIsUnion = encodeUnion consFields
    | otherwise = mkObject tyName (toFieldRes <$> consFields)
    where
      -- ENUM
      encodeUnion ::
        [FieldRep (m (ResolverValue m))] ->
        ResolverValue m
      encodeUnion [] = mkEnum consName
      -- Type References --------------------------------------------------------------
      encodeUnion [FieldRep {fieldTypeRef = TypeRef {typeConName}, fieldValue}]
        | isUnionRef tyName cons = ResUnion typeConName (fieldValue >>= requireObject)
      -- Inline Union Types ----------------------------------------------------------------------------
      encodeUnion fields = mkUnion consName (toFieldRes <$> fields)

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
  ResolverState (ResolverObject m)
objectResolvers value = requireObject (exploreResolvers value)

type EncodeConstraint (m :: * -> *) a =
  ( GQLType a,
    Generic a,
    TypeRep (Encode m) (m (ResolverValue m)) (Rep a)
  )

type EncodeObjectConstraint (o :: OperationType) e (m :: * -> *) a =
  EncodeConstraint (Resolver o e m) (a (Resolver o e m))

type EncodeConstraints e m query mut sub =
  ( ChannelsConstraint e m sub,
    EncodeObjectConstraint QUERY e m query,
    EncodeObjectConstraint MUTATION e m mut,
    EncodeObjectConstraint SUBSCRIPTION e m sub
  )

toFieldRes :: FieldRep (m (ResolverValue m)) -> ResolverEntry m
toFieldRes FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)

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

--- Named

type EncodeNamedConstraints e m query mut sub =
  ( EncodeNamedObjectConstraint QUERY e m query
  )

deriveNamedModel ::
  forall e m query mut sub.
  (Monad m, EncodeNamedConstraints e m query mut sub) =>
  NamedResolvers m e query mut sub ->
  GQLResult (RootResolverValue e m)
deriveNamedModel NamedResolvers = do
  let res = join $ toList $ traverseTypes deriveResolver (Proxy @(query (Resolver QUERY e m)))
  pure
    NamedResolversValue
      { queryResolverMap = HM.fromList $ map (\x -> (resolverName x, x)) res
      }

deriveResolver :: Mappable (DeriveNamedResolver m) [NamedResolver m] KindedProxy
deriveResolver = Mappable deriveNamedResolver

type EncodeNamedObjectConstraint (o :: OperationType) e (m :: * -> *) a =
  ( ScanConstraint
      (DeriveNamedResolver (Resolver o e m))
      (KIND (a (Resolver o e m)))
      (a (Resolver o e m)),
    GQLType (a (Resolver o e m))
  )

class DeriveNamedResolver (m :: * -> *) (k :: DerivingKind) a where
  deriveNamedResolver :: f k a -> [NamedResolver m]

instance (GQLType a, Monad m) => DeriveNamedResolver m SCALAR a where
  deriveNamedResolver _ = []

instance
  ( GQLType (a (Resolver o e m)),
    ResolveNamed (Resolver o e m) a,
    Monad m,
    LiftOperation o,
    EcondeNamed (Resolver o e m) a,
    (Generic (a (RefResolver (Resolver o e m)))),
    ( TypeRep
        NamedField
        (Resolver o e m NamedResolverField)
        (Rep (a (RefResolver (Resolver o e m))))
    )
  ) =>
  DeriveNamedResolver (Resolver o e m) TYPE (a (Resolver o e m))
  where
  deriveNamedResolver _ =
    [ NamedResolver
        { resolverName = name,
          resolver = fmap exploreNamedResolvers . resolve
        }
    ]
    where
      resolve :: ValidValue -> Resolver o e m (a (RefResolver (Resolver o e m)))
      resolve x = liftResolverState (decode x :: ResolverState (Dep a)) >>= resolveNamed
      name = getTypeName (Proxy @(a (Resolver o e m)))

class NamedField a where
  namedField :: Monad m => a -> m NamedResolverField

instance NamedField a where
  -- TODO: real field resolvers
  namedField _ = pure (ResObject (NamedResolverRef "Post" "Some Value"))

type EcondeNamed m a =
  ( MonadError GQLError m,
    GQLType (a (RefResolver m)),
    NamedField (a (RefResolver m))
  )

exploreNamedResolvers ::
  forall m a.
  ( MonadError GQLError m,
    GQLType (a (RefResolver m)),
    NamedField (a (RefResolver m)),
    (Generic (a (RefResolver m))),
    ( TypeRep
        NamedField
        (m NamedResolverField)
        (Rep (a (RefResolver m)))
    )
  ) =>
  a (RefResolver m) ->
  NamedResolverResult m
exploreNamedResolvers =
  convertNamedNode
    . toValue
      ( TypeConstraint (namedField . runIdentity) ::
          TypeConstraint NamedField (m NamedResolverField) Identity
      )
      (Proxy @OUT)

convertNamedNode ::
  MonadError GQLError m =>
  DataType (m NamedResolverField) ->
  NamedResolverResult m
convertNamedNode
  DataType
    { tyName,
      tyIsUnion,
      tyCons = ConsRep {consFields, consName}
    }
    | tyIsUnion = NamedUnionResolver (NamedResolverRef consName Null)
    | otherwise =
      NamedObjectResolver
        ObjectTypeResolver
          { __typename = tyName,
            objectFields = HM.fromList (xy <$> consFields)
          }
    where
      xy FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)

instance
  ( GQLType a,
    DeriveNamedResolver m' (KIND a) a,
    Monad m
  ) =>
  DeriveNamedResolver m' CUSTOM (Resolver o e m a)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance
  ( GQLType a,
    DeriveNamedResolver m (KIND a) a,
    Monad m
  ) =>
  DeriveNamedResolver m CUSTOM (input -> a)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance
  ( GQLType t,
    DeriveNamedResolver m (KIND t) t,
    Monad m
  ) =>
  DeriveNamedResolver m CUSTOM (TypeGuard i t)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND t) t)

instance
  ( GQLType t,
    DeriveNamedResolver m (KIND t) t,
    Monad m
  ) =>
  DeriveNamedResolver m WRAPPER (f t)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND t) t)

instance
  ( GQLType t,
    DeriveNamedResolver m (KIND t) t,
    Monad m
  ) =>
  DeriveNamedResolver m CUSTOM (Map k t)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND t) t)

getTypeName :: GQLType a => f a -> TypeName
getTypeName proxy = gqlTypeName $ __type proxy OUT
