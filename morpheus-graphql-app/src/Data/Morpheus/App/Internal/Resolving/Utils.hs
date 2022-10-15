{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Utils
  ( ResolverValue (..),
    requireObject,
    NamedResolverRef (..),
    ObjectTypeResolver,
    lookupResJSON,
    mkValue,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as A
import Data.Morpheus.App.Internal.Resolving.ResolverState
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolverRef (..),
    ObjectTypeResolver (..),
    ResolverValue (..),
    mkList,
    mkNull,
    mkObjectMaybe,
  )
import Data.Morpheus.Internal.Utils (selectOr, toAssoc)
import qualified Data.Morpheus.Internal.Utils as U
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    ScalarValue (..),
    TypeName,
    decodeScientific,
    internal,
    packName,
    unpackName,
  )
import Data.Text (breakOn)
import qualified Data.Vector as V
import Relude

lookupResJSON ::
  ( MonadError GQLError f,
    MonadReader ResolverContext f,
    MonadReader ResolverContext m
  ) =>
  FieldName ->
  A.Value ->
  f (ObjectTypeResolver m)
lookupResJSON name (A.Object fields) =
  selectOr
    mkEmptyObject
    (requireObject <=< mkValue)
    (unpackName name)
    fields
lookupResJSON _ _ = mkEmptyObject

mkEmptyObject :: Monad m => m (ObjectTypeResolver a)
mkEmptyObject = pure $ ObjectTypeResolver mempty

mkValue ::
  ( MonadReader ResolverContext f,
    MonadReader ResolverContext m
  ) =>
  A.Value ->
  f (ResolverValue m)
mkValue (A.Object v) = pure $ mkObjectMaybe typename fields
  where
    typename = U.lookup "__typename" v >>= unpackJSONName
    fields = map (bimap packName mkValue) (toAssoc v)
mkValue (A.Array ls) = mkList <$> traverse mkValue (V.toList ls)
mkValue A.Null = pure mkNull
mkValue (A.Number x) = pure $ ResScalar (decodeScientific x)
mkValue (A.String x) = maybe simple f (withSelf x)
  where
    simple = pure $ ResScalar (String x)
    f _ = do
      sel <- asks currentSelection
      pure $ ResScalar (String (show sel))
mkValue (A.Bool x) = pure $ ResScalar (Boolean x)

withSelf :: Text -> Maybe Text
withSelf txt = case breakOn "::" txt of
  ("@self", field) -> Just field
  _ -> Nothing

requireObject :: MonadError GQLError f => ResolverValue m -> f (ObjectTypeResolver m)
requireObject (ResObject _ x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

unpackJSONName :: A.Value -> Maybe TypeName
unpackJSONName (A.String x) = Just (packName x)
unpackJSONName _ = Nothing
