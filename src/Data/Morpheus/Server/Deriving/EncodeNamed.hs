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

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON (..))
import Data.Morpheus.App.Internal.Resolving
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.NamedResolvers (NamedResolverT (..), ResolveNamed (Dep, resolveNamed))
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
import Data.Morpheus.Server.Deriving.Utils.GTraversable
import Data.Morpheus.Server.Deriving.Utils.Kinded (KindedProxy (KindedProxy))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__type),
    KIND,
    TypeData (gqlTypeName),
  )
import Data.Morpheus.Types
  ( NamedResolvers (..),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    OUT,
    QUERY,
    TypeCategory (OUT),
    TypeName,
    ValidValue,
    Value (Null),
    internal,
    replaceValue,
  )
import qualified GHC.Exts as HM
import GHC.Generics
  ( Generic (..),
  )
import Relude

class Encode (m :: * -> *) res where
  encodeField :: res -> m (NamedResolverField m)

instance (EncodeFieldKind (KIND a) m a) => Encode m a where
  encodeField resolver = encodeFieldKind (ContextValue resolver :: ContextValue (KIND a) a)

class EncodeFieldKind (k :: DerivingKind) (m :: * -> *) (a :: *) where
  encodeFieldKind :: ContextValue k a -> m (NamedResolverField m)

instance (EncodeScalar a, Monad m) => EncodeFieldKind SCALAR m a where
  encodeFieldKind = pure . ResScalar . encodeScalar . unContextValue

instance (FieldConstraint m a, MonadError GQLError m) => EncodeFieldKind TYPE m a where
  encodeFieldKind (ContextValue _) = throwError (internal "types are resolved by Refs")

instance (GQLType a, Applicative m, EncodeFieldKind (KIND a) m a) => EncodeFieldKind WRAPPER m [a] where
  encodeFieldKind = fmap ResList . traverse encodeField . unContextValue

instance (GQLType a, EncodeFieldKind (KIND a) m a, Applicative m) => EncodeFieldKind WRAPPER m (Maybe a) where
  encodeFieldKind (ContextValue (Just x)) = encodeField x
  encodeFieldKind (ContextValue Nothing) = pure mkNull

instance
  ( Monad m,
    GQLType a,
    EncodeFieldKind (KIND a) m a,
    ToJSON (Dep a)
  ) =>
  EncodeFieldKind CUSTOM m (NamedResolverT m a)
  where
  encodeFieldKind = encodeRef . unContextValue
    where
      name :: TypeName
      name = getTypeName (Proxy @a)
      encodeRef :: Monad m => NamedResolverT m a -> m (NamedResolverField m)
      encodeRef (Ref x) = ResObject name . NamedResolverRef name . replaceValue . toJSON <$> x
      encodeRef (Value value) = value >>= encodeField
      encodeRef (Refs refs) = mkList . map (ResObject name . NamedResolverRef name . replaceValue . toJSON) <$> refs

instance
  ( DecodeConstraint a,
    Generic a,
    Monad m,
    Encode (Resolver o e m) b,
    LiftOperation o
  ) =>
  EncodeFieldKind CUSTOM (Resolver o e m) (a -> b)
  where
  encodeFieldKind (ContextValue f) =
    getArguments
      >>= liftResolverState . decodeArguments
      >>= encodeField . f

type FieldConstraint m a =
  ( GQLType a,
    Generic a,
    TypeRep (Encode m) (m (NamedResolverField m)) (Rep a)
  )

getFieldValues :: FieldConstraint m a => a -> DataType (m (NamedResolverField m))
getFieldValues =
  toValue
    ( TypeConstraint (encodeField . runIdentity) ::
        TypeConstraint (Encode m) (m (NamedResolverField m)) Identity
    )
    (Proxy @OUT)

convertNamedNode ::
  DataType (m (NamedResolverField m)) ->
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

-- TYPES
type EncodeNamedConstraints e m query mut sub =
  ( EncodeNamedObjectConstraint (Resolver QUERY e m) query
  )

deriveNamedModel ::
  forall e m query mut sub.
  (Monad m, EncodeNamedConstraints e m query mut sub) =>
  NamedResolvers m e query mut sub ->
  GQLResult (RootResolverValue e m)
deriveNamedModel NamedResolvers = do
  let res = join $ toList $ traverseTypes deriveResolver (Proxy @(query (NamedResolverT (Resolver QUERY e m))))
  pure $
    NamedResolversValue
      { queryResolverMap = HM.fromList $ map (\x -> (resolverName x, x)) res
      }

deriveResolver :: Mappable (DeriveNamedResolver m) [NamedResolver m] KindedProxy
deriveResolver = Mappable deriveNamedResolver

type EncodeNamedObjectConstraint m a =
  ( ScanConstraint (DeriveNamedResolver m) (KIND (a (NamedResolverT m))) (a (NamedResolverT m)),
    GQLType (a (NamedResolverT m))
  )

class DeriveNamedResolver (m :: * -> *) (k :: DerivingKind) a where
  deriveNamedResolver :: f k a -> [NamedResolver m]

instance (GQLType a, Monad m) => DeriveNamedResolver m SCALAR a where
  deriveNamedResolver _ = []

instance
  ( Monad m,
    LiftOperation o,
    Generic a,
    GQLType a,
    EncodeFieldKind (KIND a) (Resolver o e m) a,
    Decode (Dep a),
    ResolveNamed (Resolver o e m) a,
    TypeRep (Encode (Resolver o e m)) (Resolver o e m (NamedResolverField (Resolver o e m))) (Rep a)
  ) =>
  DeriveNamedResolver (Resolver o e m) TYPE (a :: *)
  where
  deriveNamedResolver _ =
    [ NamedResolver
        { resolverName = getTypeName (Proxy @a),
          resolver = fmap (convertNamedNode . getFieldValues) . resolve
        }
    ]
    where
      resolve :: ValidValue -> Resolver o e m a
      resolve x = liftResolverState (decode x :: ResolverState (Dep a)) >>= resolveNamed

instance
  (GQLType a, Monad m, DeriveNamedResolver m (KIND a) a) =>
  DeriveNamedResolver m CUSTOM (NamedResolverT m a)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance
  (GQLType a, DeriveNamedResolver m (KIND a) a, Monad m) =>
  DeriveNamedResolver m CUSTOM (input -> a)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

instance
  (GQLType a, DeriveNamedResolver m (KIND a) a, Monad m) =>
  DeriveNamedResolver m WRAPPER (f a)
  where
  deriveNamedResolver _ = deriveNamedResolver (KindedProxy :: KindedProxy (KIND a) a)

getTypeName :: GQLType a => f a -> TypeName
getTypeName proxy = gqlTypeName $ __type proxy OUT
