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

module Data.Morpheus.Server.Deriving.Named.EncodeValue
  ( EncodeFieldKind,
    Encode,
    getTypeName,
    encodeResolverValue,
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
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.NamedResolvers
  ( NamedResolverT (..),
    ResolveNamed (..),
  )
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeConstraint,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Encode
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DataType (..),
    FieldRep (..),
    TypeConstraint (..),
    TypeRep (..),
    toFieldRes,
    toValue,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__type),
    KIND,
    TypeData (gqlTypeName),
  )
import Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    OUT,
    TypeCategory (OUT),
    TypeName,
    internal,
    replaceValue,
  )
import qualified GHC.Exts as HM
import GHC.Generics
  ( Generic (..),
  )
import Relude

encodeResolverValue :: (MonadError GQLError m, FieldConstraint m a) => a -> m (NamedResolverResult m)
encodeResolverValue = convertNamedNode . getFieldValues

type FieldConstraint m a =
  ( GQLType a,
    Generic a,
    TypeRep (Encode m) (m (ResolverValue m)) (Rep a)
  )

class Encode (m :: * -> *) res where
  encodeField :: res -> m (ResolverValue m)

instance (EncodeFieldKind (KIND a) m a) => Encode m a where
  encodeField resolver = encodeFieldKind (ContextValue resolver :: ContextValue (KIND a) a)

class EncodeFieldKind (k :: DerivingKind) (m :: * -> *) (a :: *) where
  encodeFieldKind :: ContextValue k a -> m (ResolverValue m)

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
      encodeRef :: Monad m => NamedResolverT m a -> m (ResolverValue m)
      encodeRef (Ref x) = pure $ ResRef (NamedResolverRef name . replaceValue . toJSON <$> x)
      encodeRef (Value value) = value >>= encodeField
      encodeRef (Refs refs) = mkList . map (ResRef . pure . NamedResolverRef name . replaceValue . toJSON) <$> refs

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

getFieldValues :: FieldConstraint m a => a -> DataType (m (ResolverValue m))
getFieldValues =
  toValue
    ( TypeConstraint (encodeField . runIdentity) ::
        TypeConstraint (Encode m) (m (ResolverValue m)) Identity
    )
    (Proxy @OUT)

convertNamedNode ::
  MonadError GQLError m =>
  DataType (m (ResolverValue m)) ->
  m (NamedResolverResult m)
convertNamedNode
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
            { objectFields = HM.fromList (toFieldRes <$> consFields)
            }

deriveUnion :: (MonadError GQLError m) => [FieldRep (m (ResolverValue m))] -> m (NamedResolverResult m)
deriveUnion [FieldRep {..}] =
  NamedUnionResolver <$> (fieldValue >>= getRef)
deriveUnion _ = throwError "only union references are supported!"

getRef :: MonadError GQLError m => ResolverValue m -> m NamedResolverRef
getRef (ResRef x) = x
getRef _ = throwError "only resolver references are supported!"

getTypeName :: GQLType a => f a -> TypeName
getTypeName proxy = gqlTypeName $ __type proxy OUT
