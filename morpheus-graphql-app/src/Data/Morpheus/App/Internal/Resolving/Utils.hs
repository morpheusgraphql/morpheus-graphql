{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Utils
  ( ResolverValue (..),
    requireObject,
    NamedResolverRef (..),
    ObjectTypeResolver,
    lookupResJSON,
    mkValue,
    ResolverMonad,
    withField,
    withObject,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (Value (..))
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    updateCurrentType,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolverRef (..),
    ObjectTypeResolver (..),
    ResolverValue (..),
    mkBoolean,
    mkList,
    mkNull,
    mkObjectMaybe,
    mkString,
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Utils (IsMap (..), selectOr, toAssoc, (<:>))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeDefinition (..),
    TypeName,
    UnionTag (..),
    VALID,
    decodeScientific,
    internal,
    packName,
    unpackName,
  )
import Data.Morpheus.Types.SelectionTree (SelectionTree (..))
import Data.Text (breakOnEnd, splitOn)
import Relude hiding (break)

type ResolverMonad m = (MonadError GQLError m, MonadReader ResolverContext m)

lookupResJSON ::
  (ResolverMonad f, MonadReader ResolverContext m) =>
  FieldName ->
  Value ->
  f (ObjectTypeResolver m)
lookupResJSON name (Object fields) =
  selectOr
    mkEmptyObject
    (requireObject <=< mkValue)
    (unpackName name)
    fields
lookupResJSON _ _ = mkEmptyObject

mkEmptyObject :: (Monad m) => m (ObjectTypeResolver a)
mkEmptyObject = pure $ ObjectTypeResolver mempty

mkValue ::
  ( MonadReader ResolverContext f,
    MonadReader ResolverContext m
  ) =>
  Value ->
  f (ResolverValue m)
mkValue (Object v) = pure $ mkObjectMaybe typename fields
  where
    typename = lookup "__typename" v >>= unpackJSONName
    fields = map (bimap packName mkValue) (toAssoc v)
mkValue (Array ls) = mkList <$> traverse mkValue (toList ls)
mkValue Null = pure mkNull
mkValue (Number x) = pure $ ResScalar (decodeScientific x)
mkValue (String txt) = case withSelf txt of
  ARG name -> do
    sel <- asks currentSelection
    mkValue (fromMaybe Null (getArgument name sel))
  NoAPI v -> pure $ mkString v
mkValue (Bool x) = pure $ mkBoolean x

data SelfAPI
  = ARG Text
  | NoAPI Text

withSelf :: Text -> SelfAPI
withSelf txt = case breakOnEnd "::" txt of
  ("@SELF::", field) -> case splitOn "." field of
    ["ARG", name] -> ARG name
    _ -> NoAPI txt
  _ -> NoAPI txt

requireObject :: (MonadError GQLError f) => ResolverValue m -> f (ObjectTypeResolver m)
requireObject (ResObject _ x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

unpackJSONName :: Value -> Maybe TypeName
unpackJSONName (String x) = Just (packName x)
unpackJSONName _ = Nothing

withField :: (Monad m') => a -> (m (ResolverValue m) -> m' a) -> FieldName -> ObjectTypeResolver m -> m' a
withField fb suc selectionName ObjectTypeResolver {..} = maybe (pure fb) suc (lookup selectionName objectFields)

withObject ::
  (ResolverMonad m) =>
  Maybe TypeName ->
  (Maybe (SelectionSet VALID) -> m value) ->
  SelectionContent VALID ->
  m value
withObject __typename f = updateCurrentType __typename . checkContent
  where
    checkContent (SelectionSet selection) = f (Just selection)
    checkContent (UnionSelection interface unionSel) = do
      typename <- asks (typeName . currentType)
      selection <- selectOr (pure interface) (fx interface) typename unionSel
      f selection
      where
        fx (Just x) y = Just <$> (x <:> unionTagSelection y)
        fx Nothing y = pure $ Just $ unionTagSelection y
    checkContent SelectionField = do
      sel <- asks currentSelection
      throwError $ subfieldsNotSelected (selectionName sel) "" (selectionPosition sel)
