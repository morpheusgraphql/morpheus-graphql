{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.ResolveValue
  ( resolveRef,
    resolveObject,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    askFieldTypeName,
    updateCurrentType,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    ResolverMap,
    ResolverValue (..),
    mkEnum,
    mkUnion,
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Utils ((<:>), KeyOf (keyOf), selectOr, traverseCollection)
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    GQLError,
    Msg (msg),
    ObjectEntry (ObjectEntry),
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeDefinition (..),
    TypeName,
    UnionTag (unionTagSelection),
    VALID,
    VALID,
    ValidValue,
    ValidValue,
    Value (..),
    internal,
    unitFieldName,
    unitTypeName,
    unpackName,
  )
import Relude

resolveSelection ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverMap m ->
  ResolverValue m ->
  SelectionContent VALID ->
  m ValidValue
resolveSelection rmap (ResLazy x) selection =
  x >>= flip (resolveSelection rmap) selection
resolveSelection rmap (ResList xs) selection =
  List <$> traverse (flip (resolveSelection rmap) selection) xs
-- Object -----------------
resolveSelection rmap (ResObject tyName obj) sel = withObject tyName (resolveObject rmap obj) sel
-- ENUM
resolveSelection _ (ResEnum name) SelectionField = pure $ Scalar $ String $ unpackName name
resolveSelection rmap (ResEnum name) unionSel@UnionSelection {} =
  resolveSelection rmap (mkUnion name [(unitFieldName, pure $ mkEnum unitTypeName)]) unionSel
resolveSelection _ ResEnum {} _ = throwError (internal "wrong selection on enum value")
-- SCALARS
resolveSelection _ ResNull _ = pure Null
resolveSelection _ (ResScalar x) SelectionField = pure $ Scalar x
resolveSelection _ ResScalar {} _ =
  throwError (internal "scalar Resolver should only receive SelectionField")
resolveSelection rmap (ResRef ref) sel = ref >>= flip (resolveRef rmap) sel

withObject ::
  ( Monad m,
    MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  Maybe TypeName ->
  (SelectionSet VALID -> m value) ->
  SelectionContent VALID ->
  m value
withObject __typename f = updateCurrentType __typename . checkContent
  where
    checkContent (SelectionSet selection) = f selection
    checkContent (UnionSelection interface unionSel) = do
      typename <- asks (typeName . currentType)
      selection <- selectOr (pure interface) ((interface <:>) . unionTagSelection) typename unionSel
      f selection
    checkContent _ = do
      sel <- asks currentSelection
      throwError $ subfieldsNotSelected (selectionName sel) "" (selectionPosition sel)

resolveRef ::
  ( MonadError GQLError m,
    MonadReader ResolverContext m
  ) =>
  ResolverMap m ->
  NamedResolverRef ->
  SelectionContent VALID ->
  m ValidValue
resolveRef rmap ref selection = do
  namedResolver <- getNamedResolverBy ref rmap
  case namedResolver of
    NamedObjectResolver res -> withObject (Just (resolverTypeName ref)) (resolveObject rmap res) selection
    NamedUnionResolver unionRef -> resolveSelection rmap (ResRef $ pure unionRef) selection
    NamedEnumResolver value -> resolveSelection rmap (ResEnum value) selection

getNamedResolverBy ::
  (MonadError GQLError m) =>
  NamedResolverRef ->
  ResolverMap m ->
  m (NamedResolverResult m)
getNamedResolverBy ref = selectOr cantFoundError ((resolverArgument ref &) . resolver) (resolverTypeName ref)
  where
    cantFoundError = throwError ("Resolver Type " <> msg (resolverTypeName ref) <> "can't found")

resolveObject ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverMap m ->
  ObjectTypeResolver m ->
  SelectionSet VALID ->
  m ValidValue
resolveObject rmap drv =
  fmap Object . traverseCollection resolver
  where
    resolver currentSelection = do
      t <- askFieldTypeName (selectionName currentSelection)
      updateCurrentType t $ local (\ctx -> ctx {currentSelection}) $
        ObjectEntry (keyOf currentSelection)
          <$> runFieldResolver rmap currentSelection drv

runFieldResolver ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverMap m ->
  Selection VALID ->
  ObjectTypeResolver m ->
  m ValidValue
runFieldResolver rmap Selection {selectionName, selectionContent}
  | selectionName == "__typename" =
    const (Scalar . String . unpackName <$> asks (typeName . currentType))
  | otherwise =
    maybe (pure Null) (>>= \x -> resolveSelection rmap x selectionContent)
      . HM.lookup selectionName
      . objectFields
