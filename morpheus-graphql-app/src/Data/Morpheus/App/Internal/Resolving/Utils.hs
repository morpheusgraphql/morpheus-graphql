{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Utils
  ( ResolverValue (..),
    mkEnum,
    mkBoolean,
    mkFloat,
    mkInt,
    mkList,
    mkNull,
    mkString,
    unpackJSONName,
    resolveResolverDefinition,
    resolveObjectTypeResolver,
    requireObject,
    NamedResolverRef (..),
    ObjectTypeResolver,
    mkObject,
    mkObject',
    lookupResJSON,
    resolveObject,
    mkValue,
    mkUnion,
    ResolverEntry,
    ResolverMap,
    runResolverMap,
    NamedResolver (..),
    NamedResolverResult (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.Event (EventHandler (Channel))
import Data.Morpheus.App.Internal.Resolving.Resolver (LiftOperation, Resolver, ResponseStream, runResolver)
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
  )
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    ResolverMap,
    ResolverValue (..),
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Ext ((<:>))
import Data.Morpheus.Internal.Utils (KeyOf (keyOf), selectOr, traverseCollection)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    FieldName,
    GQLError,
    GQLError,
    Msg (msg),
    ObjectEntry (..),
    ScalarValue (..),
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName,
    TypeName,
    UnionTag (..),
    VALID,
    VALID,
    ValidValue,
    ValidValue,
    Value (..),
    decodeScientific,
    internal,
    packName,
    packName,
    unitFieldName,
    unitTypeName,
    unpackName,
  )
import qualified Data.Vector as V
import Relude

mkObject ::
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObject __typename = ResObject Nothing . mkObject' __typename

mkObjectMaybe ::
  Maybe TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObjectMaybe __typename =
  ResObject __typename
    . mkObject'
      (fromMaybe "UNKNOWN" __typename)

lookupResJSON :: (MonadError GQLError f, Monad m) => Text -> A.Value -> f (ObjectTypeResolver m)
lookupResJSON name (A.Object fields) =
  selectOr
    (mkEmptyObject name)
    (requireObject . mkValue)
    name
    fields
lookupResJSON name _ = mkEmptyObject name

mkEmptyObject :: Monad m => Text -> m (ObjectTypeResolver a)
mkEmptyObject name = pure $ ObjectTypeResolver (packName name) mempty

mkObject' ::
  TypeName ->
  [(FieldName, m (ResolverValue m))] ->
  ObjectTypeResolver m
mkObject' __typename fields =
  ( ObjectTypeResolver
      { __typename,
        objectFields = HM.fromList fields
      }
  )

mkValue ::
  (Monad m) =>
  A.Value ->
  ResolverValue m
mkValue (A.Object v) =
  mkObjectMaybe
    (unpackJSONName <$> HM.lookup "__typename" v)
    $ fmap
      (bimap packName (pure . mkValue))
      (HM.toList v)
mkValue (A.Array ls) = mkList (fmap mkValue (V.toList ls))
mkValue A.Null = mkNull
mkValue (A.Number x) = ResScalar (decodeScientific x)
mkValue (A.String x) = ResScalar (String x)
mkValue (A.Bool x) = ResScalar (Boolean x)

mkUnion ::
  (Monad m) =>
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkUnion name fields =
  ResObject
    (Just name)
    $ ObjectTypeResolver
      { __typename = name,
        objectFields = HM.fromList fields
      }

resolveObjectTypeResolver ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (m (ResolverValue m) -> m ValidValue) ->
  ObjectTypeResolver m ->
  SelectionSet VALID ->
  m ValidValue
resolveObjectTypeResolver f drv@ObjectTypeResolver {__typename} =
  fmap Object . traverseCollection resolver
  where
    resolver currentSelection =
      local (\ctx -> ctx {currentSelection, currentTypeName = __typename}) $
        ObjectEntry (keyOf currentSelection)
          <$> runFieldResolver
            f
            currentSelection
            drv

runFieldResolver ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (m (ResolverValue m) -> m ValidValue) ->
  Selection VALID ->
  ObjectTypeResolver m ->
  m ValidValue
runFieldResolver f Selection {selectionName}
  | selectionName == "__typename" =
    pure . Scalar . String . unpackName . __typename
  | otherwise =
    maybe (pure Null) f
      . HM.lookup selectionName
      . objectFields

withObject ::
  ( Monad m,
    MonadError GQLError m
  ) =>
  TypeName ->
  (SelectionSet VALID -> m value) ->
  Selection VALID ->
  m value
withObject __typename f Selection {selectionName, selectionContent, selectionPosition} = checkContent selectionContent
  where
    checkContent (SelectionSet selection) = f selection
    checkContent (UnionSelection interface unionSel) = do
      selection <- selectOr (pure interface) ((interface <:>) . unionTagSelection) __typename unionSel
      f selection
    checkContent _ = throwError $ subfieldsNotSelected selectionName "" selectionPosition

requireObject :: MonadError GQLError f => ResolverValue m -> f (ObjectTypeResolver m)
requireObject (ResObject _ x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

mkString :: Text -> ResolverValue m
mkString = ResScalar . String

mkFloat :: Double -> ResolverValue m
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValue m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValue m
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValue m] -> ResolverValue m
mkList = ResList

mkNull :: ResolverValue m
mkNull = ResNull

unpackJSONName :: A.Value -> TypeName
unpackJSONName (A.String x) = packName x
unpackJSONName _ = "__JSON__"

mkEnum :: TypeName -> ResolverValue m
mkEnum = ResEnum

resolveResolverDefinition ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverMap m ->
  ResolverValue m ->
  m ValidValue
resolveResolverDefinition rmap res =
  asks currentSelection
    >>= encodeResolverDefinition
      rmap
      res
      . selectionContent

resolveObject ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverMap m ->
  ObjectTypeResolver m ->
  SelectionSet VALID ->
  m ValidValue
resolveObject rmap = resolveObjectTypeResolver (>>= resolveResolverDefinition rmap)

type ResolverEntry m = (FieldName, m (ResolverValue m))

mkEnumNull :: (Monad m) => [ResolverEntry m]
mkEnumNull = [(unitFieldName, pure $ mkEnum unitTypeName)]

-- LIST
encodeResolverDefinition ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverMap m ->
  ResolverValue m ->
  SelectionContent VALID ->
  m ValidValue
encodeResolverDefinition rmap = encodeResolverDefinition'
  where
    encodeResolverDefinition' (ResLazy x) sel =
      x >>= (`encodeResolverDefinition'` sel)
    encodeResolverDefinition' (ResList xs) selection =
      List <$> traverse (`encodeResolverDefinition'` selection) xs
    -- Object -----------------
    encodeResolverDefinition' (ResObject tyName obj) _ = do
      currentTypeName <- asks currentTypeName
      asks currentSelection >>= withObject (fromMaybe currentTypeName tyName) (resolveObject rmap obj)
    -- ENUM
    encodeResolverDefinition' (ResEnum enum) SelectionField = handle enum
      where
        handle x = pure $ Scalar $ String $ unpackName x
    encodeResolverDefinition' (ResEnum name) unionSel@UnionSelection {} =
      encodeResolverDefinition' (mkUnion name mkEnumNull) unionSel
    encodeResolverDefinition' ResEnum {} _ = throwError (internal "wrong selection on enum value")
    -- SCALARS
    encodeResolverDefinition' ResNull _ = pure Null
    encodeResolverDefinition' (ResScalar x) SelectionField = pure $ Scalar x
    encodeResolverDefinition' ResScalar {} _ =
      throwError (internal "scalar Resolver should only receive SelectionField")
    encodeResolverDefinition' (ResRef mRef) sel = do
      ref <- mRef
      resolveRef rmap ref sel

runResolverMap ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  TypeName ->
  ResolverMap (Resolver o e m) ->
  ResolverContext ->
  SelectionSet VALID ->
  ResponseStream e m ValidValue
runResolverMap
  channels
  name
  res
  ctx
  selection = runResolver channels resolvedValue ctx
    where
      resolvedValue = resolveRef res (NamedResolverRef name Null) (SelectionSet selection)

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
    NamedObjectResolver res -> do
      sel <- asks currentSelection
      withObject (resolverTypeName ref) (resolveObjectTypeResolver (>>= resolveResolverDefinition rmap) res) sel
    NamedUnionResolver unionRef -> encodeResolverDefinition rmap (ResRef $ pure unionRef) selection
    NamedEnumResolver value -> encodeResolverDefinition rmap (ResEnum value) selection

getNamedResolverBy ::
  (MonadError GQLError m) =>
  NamedResolverRef ->
  ResolverMap m ->
  m (NamedResolverResult m)
getNamedResolverBy ref = selectOr cantFoundError ((resolverArgument ref &) . resolver) (resolverTypeName ref)
  where
    cantFoundError = throwError ("Resolver Type " <> msg (resolverTypeName ref) <> "can't found")
