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
    FieldConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON (..), Value)
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
import Data.Morpheus.Server.Deriving.Decode
  ( Decode,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Encode
  ( ContextValue (..),
  )
import Data.Morpheus.Server.Deriving.Schema.Directive (toFieldRes)
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DataType (..),
    FieldRep (..),
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveValueOptions (..),
    DeriveWith,
    deriveValue,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
import Data.Morpheus.Server.NamedResolvers
  ( NamedRef,
    NamedResolverT (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__type),
    KIND,
    deriveTypename,
    __typeData,
  )
import Data.Morpheus.Server.Types.Internal
  ( TypeData (gqlTypeName),
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
    TypeCategory (OUT),
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

encodeResolverValue :: (MonadError GQLError m, FieldConstraint m a) => [Maybe a] -> m [NamedResolverResult m]
encodeResolverValue x = traverse encodeNode x
  where
    encodeNode (Just v) = convertNamedNode (Identity x) (getFieldValues v)
    encodeNode Nothing = pure NamedNullResolver

type FieldConstraint m a =
  ( GQLType a,
    Generic a,
    DeriveWith (GValueMapConstraint m) (m (ResolverValue m)) (Rep a)
  )

class Encode (m :: Type -> Type) res where
  encodeField :: res -> m (ResolverValue m)

instance (EncodeFieldKind (KIND a) m a) => Encode m a where
  encodeField resolver = encodeFieldKind (ContextValue resolver :: ContextValue (KIND a) a)

class EncodeFieldKind (k :: DerivingKind) (m :: Type -> Type) (a :: Type) where
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
    ToJSON (NamedRef a)
  ) =>
  EncodeFieldKind CUSTOM m (NamedResolverT m a)
  where
  encodeFieldKind = encodeRef . unContextValue
    where
      name :: TypeName
      name = getTypeName (Proxy @a)
      encodeRef :: Monad m => NamedResolverT m a -> m (ResolverValue m)
      encodeRef (NamedResolverT ref) = do
        value <- replaceValue . toJSON <$> ref
        case value of
          (List ls) -> pure $ mkList $ map (packRef name) ls
          _ -> pure $ packRef name value

packRef :: Applicative m => TypeName -> ValidValue -> ResolverValue m
packRef name v = ResRef $ pure $ NamedResolverRef name [v]

instance
  ( Decode a,
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

class (Encode m a, GQLType a) => GValueMapConstraint m a

instance (Encode m a, GQLType a) => GValueMapConstraint m a

getFieldValues :: forall m a. FieldConstraint m a => a -> DataType (m (ResolverValue m))
getFieldValues =
  deriveValue
    ( DeriveValueOptions
        { __valueApply = encodeField,
          __valueTypeName = deriveTypename (KindedProxy :: KindedProxy OUT a),
          __valueGetType = __typeData . kinded (Proxy @OUT)
        } ::
        DeriveValueOptions OUT (GValueMapConstraint m) (m (ResolverValue m))
    )

convertNamedNode ::
  (GQLType a, MonadError GQLError m) =>
  f a ->
  DataType (m (ResolverValue m)) ->
  m (NamedResolverResult m)
convertNamedNode
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
              { objectFields = HM.fromList (toFieldRes proxy <$> consFields)
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
