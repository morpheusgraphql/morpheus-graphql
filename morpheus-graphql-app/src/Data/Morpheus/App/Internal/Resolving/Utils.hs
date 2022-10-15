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
import Data.Morpheus.Types.SelectionTree (SelectionTree (..))
import Data.Text (breakOnEnd, splitOn)
import qualified Data.Vector as V
import Relude hiding (break)

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
mkValue (A.String txt) = case withSelf txt of
  ARG name -> do
    sel <- asks currentSelection
    mkValue (fromMaybe A.Null (getArgument name sel))
  NoAPI v -> pure $ ResScalar (String v)
mkValue (A.Bool x) = pure $ ResScalar (Boolean x)

data SelfAPI
  = ARG Text
  | NoAPI Text

withSelf :: Text -> SelfAPI
withSelf txt = case breakOnEnd "::" txt of
  ("@SELF::", field) -> case splitOn "." field of
    ["ARG", name] -> ARG name
    _ -> NoAPI txt
  _ -> NoAPI txt

requireObject :: MonadError GQLError f => ResolverValue m -> f (ObjectTypeResolver m)
requireObject (ResObject _ x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

unpackJSONName :: A.Value -> Maybe TypeName
unpackJSONName (A.String x) = Just (packName x)
unpackJSONName _ = Nothing
