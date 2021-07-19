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
  ( ResolverValue (..),
    ResolverObject (..),
    ResolverEntry,
    resolveObject,
    mkUnion,
    mkEnum,
    mkEnumNull,
    mkObject,
    mkBoolean,
    mkFloat,
    mkInt,
    mkList,
    mkNull,
    mkString,
    mkValue,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Ext
  ( (<:>),
    Merge (..),
  )
import Data.Morpheus.Internal.Utils
  ( keyOf,
    selectOr,
    traverseCollection,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    ObjectEntry (..),
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    Token,
    TypeName,
    UnionTag (..),
    VALID,
    ValidValue,
    ValidationError,
    ValidationError,
    Value (..),
    Value (..),
    decodeScientific,
    internal,
    msgInternal,
    packName,
    unitFieldName,
    unitTypeName,
    unpackName,
  )
import qualified Data.Vector as V
import Relude hiding (Show, empty)
import Prelude (Show (..))

data ResolverValue (m :: * -> *)
  = ResNull
  | ResScalar ScalarValue
  | ResEnum TypeName
  | ResObject (ResolverObject m)
  | ResList [ResolverValue m]
  | ResUnion TypeName (m (ResolverValue m))

instance Show (ResolverValue m) where
  show _ = "TODO:"

instance
  ( Monad f,
    Monad m,
    MonadError ValidationError f,
    MonadError ValidationError m
  ) =>
  Merge f (ResolverValue m)
  where
  merge ResNull ResNull = pure ResNull
  merge ResScalar {} x@ResScalar {} = pure x
  merge ResEnum {} x@ResEnum {} = pure x
  merge (ResObject x) (ResObject y) = ResObject <$> merge x y
  merge _ _ = throwError (internal "can't merge: incompatible resolvers")

type ResolverEntry m = (FieldName, m (ResolverValue m))

data ResolverObject m = ResolverObject
  { __typename :: TypeName,
    objectFields :: HashMap FieldName (m (ResolverValue m))
  }

instance Show (ResolverObject m) where
  show _ = "TODO:"

instance
  ( Monad m,
    Applicative f,
    MonadError ValidationError m
  ) =>
  Merge f (ResolverObject m)
  where
  merge (ResolverObject tyname x) (ResolverObject _ y) =
    pure $ ResolverObject tyname (HM.unionWith mergeResolver x y)

mergeResolver ::
  (Monad m, Merge m a) =>
  m a ->
  m a ->
  m a
mergeResolver a b = do
  a' <- a
  b >>= merge a'

lookupRes ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError ValidationError m
  ) =>
  Selection VALID ->
  ResolverObject m ->
  m ValidValue
lookupRes Selection {selectionName}
  | selectionName == "__typename" =
    pure . Scalar . String . unpackName . __typename
  | otherwise =
    maybe
      (pure Null)
      (>>= runDataResolver)
      . HM.lookup selectionName
      . objectFields

mkUnion ::
  (Monad m) =>
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkUnion name =
  ResUnion
    name
    . pure
    . mkObject
      name

mkEnum :: TypeName -> ResolverValue m
mkEnum = ResEnum

mkEnumNull :: (Monad m) => [ResolverEntry m]
mkEnumNull = [(unitFieldName, pure $ mkEnum unitTypeName)]

mkObject ::
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObject __typename fields =
  ResObject
    ( ResolverObject
        { __typename,
          objectFields = HM.fromList fields
        }
    )

__encode ::
  forall m.
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError ValidationError m
  ) =>
  ResolverValue m ->
  Selection VALID ->
  m (Value VALID)
__encode obj sel@Selection {selectionContent} = encodeNode obj selectionContent
  where
    -- LIST
    encodeNode ::
      ResolverValue m ->
      SelectionContent VALID ->
      m (Value VALID)
    encodeNode (ResList x) _ = List <$> traverse runDataResolver x
    -- Object -----------------
    encodeNode objDrv@(ResObject ResolverObject {__typename}) _ = withObject __typename (`resolveObject` objDrv) sel
    -- ENUM
    encodeNode (ResEnum enum) SelectionField = pure $ Scalar $ String $ unpackName enum
    encodeNode (ResEnum name) unionSel@UnionSelection {} =
      encodeNode (mkUnion name mkEnumNull) unionSel
    encodeNode ResEnum {} _ = throwError (internal "wrong selection on enum value")
    -- UNION
    encodeNode (ResUnion typename unionRef) (UnionSelection interface selections) = do
      unionRes <- unionRef
      selection <- selectOr (pure interface) ((interface <:>) . unionTagSelection) typename selections
      resolveObject selection unionRes
    encodeNode (ResUnion _ unionRef) (SelectionSet selection) =
      unionRef >>= resolveObject selection
    encodeNode (ResUnion name _) SelectionField =
      throwError (internal $ "union Resolver " <> msgInternal name <> " cant resolve  SelectionField")
    -- SCALARS
    encodeNode ResNull _ = pure Null
    encodeNode (ResScalar x) SelectionField = pure $ Scalar x
    encodeNode ResScalar {} _ =
      throwError (internal "scalar Resolver should only receive SelectionField")

runDataResolver ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError ValidationError m
  ) =>
  ResolverValue m ->
  m ValidValue
runDataResolver res = asks currentSelection >>= __encode res

withObject ::
  ( Monad m,
    MonadError ValidationError m
  ) =>
  TypeName ->
  (SelectionSet VALID -> m value) ->
  Selection VALID ->
  m value
withObject __typename f Selection {selectionName, selectionContent, selectionPosition} = checkContent selectionContent
  where
    checkContent (SelectionSet selection) = f selection
    checkContent (UnionSelection interfaceSel unionSel) =
      f (selectOr interfaceSel unionTagSelection __typename unionSel)
    checkContent _ = throwError $ subfieldsNotSelected selectionName "" selectionPosition

resolveObject ::
  forall m.
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError ValidationError m
  ) =>
  SelectionSet VALID ->
  ResolverValue m ->
  m ValidValue
resolveObject selectionSet (ResObject drv@ResolverObject {__typename}) =
  Object <$> traverseCollection resolver selectionSet
  where
    resolver :: Selection VALID -> m (ObjectEntry VALID)
    resolver currentSelection =
      local (\ctx -> ctx {currentSelection, currentTypeName = __typename}) $
        ObjectEntry (keyOf currentSelection) <$> lookupRes currentSelection drv
resolveObject _ _ = throwError (internal "expected object as resolver")

mkString :: Token -> ResolverValue m
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

unPackName :: A.Value -> TypeName
unPackName (A.String x) = packName x
unPackName _ = "__JSON__"

mkValue ::
  (Monad m) =>
  A.Value ->
  ResolverValue m
mkValue (A.Object v) =
  mkObject
    (maybe "__JSON__" unPackName $ HM.lookup "__typename" v)
    $ fmap
      (bimap packName (pure . mkValue))
      (HM.toList v)
mkValue (A.Array ls) = mkList (fmap mkValue (V.toList ls))
mkValue A.Null = mkNull
mkValue (A.Number x) = ResScalar (decodeScientific x)
mkValue (A.String x) = ResScalar (String x)
mkValue (A.Bool x) = ResScalar (Boolean x)
