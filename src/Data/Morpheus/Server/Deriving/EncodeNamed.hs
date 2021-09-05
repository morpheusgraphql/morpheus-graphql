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

module Data.Morpheus.Server.Deriving.EncodeNamed
  ( deriveNamedModel,
    EncodeNamedConstraints,
  )
where

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
import Data.Morpheus.NamedResolvers (RefResolver (..), ResolveNamed (Dep, resolveNamed))
import Data.Morpheus.Server.Deriving.Decode
  ( Decode (decode),
    DecodeConstraint,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Encode (ContextValue (..))
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DataType (..),
    TypeConstraint (..),
    TypeRep (..),
    toFieldRes,
    toValue,
  )
import Data.Morpheus.Server.Deriving.Utils.GTraversble
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (KindedProxy))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__type),
    KIND,
    TypeData (gqlTypeName),
  )
import Data.Morpheus.Server.Types.Types
  ( Pair (..),
    TypeGuard (..),
  )
import Data.Morpheus.Types
  ( NamedResolvers (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( OUT,
    OperationType,
    QUERY,
    TypeCategory (OUT),
    TypeName,
    ValidValue,
    Value (Null),
  )
import qualified GHC.Exts as HM
import GHC.Generics
  ( Generic (..),
  )
import Relude

class Encode (m :: * -> *) resolver where
  encode :: resolver -> m NamedResolverField

instance (EncodeKind (KIND a) m a) => Encode m a where
  encode resolver = encodeKind (ContextValue resolver :: ContextValue (KIND a) a)

class EncodeKind (k :: DerivingKind) (m :: * -> *) (a :: *) where
  encodeKind :: ContextValue k a -> m NamedResolverField

instance
  ( -- EncodeWrapper f,
    Encode m a,
    Monad m
  ) =>
  EncodeKind WRAPPER m (f a)
  where
  encodeKind = undefined -- encodeWrapper encode . unContextValue

instance
  ( EncodeScalar a,
    Monad m
  ) =>
  EncodeKind SCALAR m a
  where
  encodeKind = pure . ResScalar . encodeScalar . unContextValue

instance (EncodeConstraint m a) => EncodeKind TYPE m a where
  encodeKind = undefined --  pure . ResObject . exploreNamedResolvers . unContextValue

--  Tuple  (a,b)
instance
  Encode m (Pair k v) =>
  EncodeKind CUSTOM m (k, v)
  where
  encodeKind = encode . uncurry Pair . unContextValue

--  Map
instance (Monad m, Encode m [Pair k v]) => EncodeKind CUSTOM m (Map k v) where
  encodeKind = encode . fmap (uncurry Pair) . M.toList . unContextValue

instance
  ( Monad m,
    GQLType a,
    EncodeKind (KIND a) m a
  ) =>
  EncodeKind CUSTOM m (RefResolver m a)
  where
  encodeKind = encodeRef
    where
      uid :: ValidValue
      uid = "PostID"
      name :: TypeName
      name = getTypeName (Proxy @a)
      encodeRef :: Monad m => ContextValue kind (RefResolver m a) -> m NamedResolverField
      encodeRef (ContextValue (Ref x)) = pure (ResObject (NamedResolverRef name uid))
      encodeRef (ContextValue (MaybeRef x)) = pure (ResObject (NamedResolverRef name uid))
      encodeRef (ContextValue (Refs x)) = pure (ResList [ResObject (NamedResolverRef name uid)])
      encodeRef (ContextValue (Val value)) = value >>= encode

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

type EncodeConstraint (m :: * -> *) a =
  ( GQLType a,
    Generic a
  )

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
    ( EncodeKind
        (KIND (a (RefResolver (Resolver o e m))))
        (Resolver o e m)
        (a (RefResolver (Resolver o e m)))
    ),
    ( TypeRep
        (Encode (Resolver o e m))
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

type EcondeNamed m a =
  ( GQLType (a (RefResolver m)),
    Encode (RefResolver m) (a (RefResolver m))
  )

exploreNamedResolvers ::
  forall m a.
  ( GQLType (a (RefResolver m)),
    Encode m (a (RefResolver m)),
    (Generic (a (RefResolver m))),
    ( TypeRep
        (Encode m)
        (m NamedResolverField)
        (Rep (a (RefResolver m)))
    )
  ) =>
  a (RefResolver m) ->
  NamedResolverResult m
exploreNamedResolvers =
  convertNamedNode
    . toValue
      ( TypeConstraint (encode . runIdentity) ::
          TypeConstraint (Encode m) (m NamedResolverField) Identity
      )
      (Proxy @OUT)

convertNamedNode ::
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
            objectFields = HM.fromList (toFieldRes <$> consFields)
          }

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
