{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.ResolverValue
  ( ResolverValue,
    ResolverEntry,
    resolveObject,
    mkUnion,
    mkObject,
    mkValue,
    lookupResJSON,
    ResolverObject,
    mkObject',
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
  )
import Data.Morpheus.App.Internal.Resolving.Utils
  ( EncoderContext (..),
    ObjectTypeResolver (..),
    ResolverValueDefinition (..),
    mkEnum,
    mkList,
    mkNull,
    requireObject,
    resolveObjectTypeResolver,
    resolveResolverDefinition,
    unpackJSONName,
  )
import Data.Morpheus.Internal.Ext
  ( Merge (..),
  )
import Data.Morpheus.Internal.Utils
  ( selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    ScalarValue (..),
    SelectionSet,
    TypeName,
    VALID,
    ValidValue,
    decodeScientific,
    packName,
    unitFieldName,
    unitTypeName,
  )
import qualified Data.Vector as V
import Relude hiding (Show, empty)

type ResolverValue (m :: * -> *) = ResolverValueDefinition (ResolverObject m) m

newtype FieldValue m = FieldValue {unFieldValue :: m (ResolverValue m)}

instance (MonadError GQLError m) => Semigroup (FieldValue m) where
  FieldValue a <> FieldValue b = FieldValue $ (,) <$> a <*> b >>= uncurry merge

type ResolverEntry m = (FieldName, m (ResolverValue m))

type ResolverObject m = ObjectTypeResolver (FieldValue m)

instance
  ( Monad m,
    Applicative f,
    MonadError GQLError m
  ) =>
  Merge f (ResolverObject m)
  where
  merge (ObjectTypeResolver typeName x) (ObjectTypeResolver _ y) =
    pure $ ObjectTypeResolver typeName (HM.unionWith (<>) x y)

resolveObject ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ObjectTypeResolver (FieldValue m) ->
  SelectionSet VALID ->
  m ValidValue
resolveObject = resolveObjectTypeResolver (unFieldValue >=> encodeResolver)

encodeResolver ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverValue m ->
  m ValidValue
encodeResolver =
  resolveResolverDefinition
    EncoderContext
      { encodeNode = resolveObject,
        mkEnumUnion = (`mkUnion` mkEnumNull)
      }

mkObject ::
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObject __typename = ResObject __typename . mkObject' __typename

mkUnion ::
  (Monad m) =>
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkUnion name fields =
  ResUnion
    name
    $ pure
    $ ObjectTypeResolver
      { __typename = name,
        objectFields = HM.fromList (map (second FieldValue) fields)
      }

mkEnumNull :: (Monad m) => [ResolverEntry m]
mkEnumNull = [(unitFieldName, pure $ mkEnum unitTypeName)]

mkValue ::
  (Monad m) =>
  A.Value ->
  ResolverValue m
mkValue (A.Object v) =
  mkObject
    (maybe "__JSON__" unpackJSONName $ HM.lookup "__typename" v)
    $ fmap
      (bimap packName (pure . mkValue))
      (HM.toList v)
mkValue (A.Array ls) = mkList (fmap mkValue (V.toList ls))
mkValue A.Null = mkNull
mkValue (A.Number x) = ResScalar (decodeScientific x)
mkValue (A.String x) = ResScalar (String x)
mkValue (A.Bool x) = ResScalar (Boolean x)

mkObject' ::
  TypeName ->
  [(FieldName, m (ResolverValue m))] ->
  ObjectTypeResolver (FieldValue m)
mkObject' __typename fields =
  ( ObjectTypeResolver
      { __typename,
        objectFields = HM.fromList (map (second FieldValue) fields)
      }
  )

lookupResJSON :: (MonadError GQLError f, Monad m) => Text -> A.Value -> f (ResolverObject m)
lookupResJSON name (A.Object fields) =
  selectOr
    (mkEmptyObject name)
    (requireObject . mkValue)
    name
    fields
lookupResJSON name _ = mkEmptyObject name

mkEmptyObject :: Monad m => Text -> m (ResolverObject a)
mkEmptyObject name = pure $ ObjectTypeResolver (packName name) mempty
