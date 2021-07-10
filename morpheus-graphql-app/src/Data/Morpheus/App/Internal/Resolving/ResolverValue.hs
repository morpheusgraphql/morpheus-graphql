{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Ext
  ( (<:>),
    SemigroupM (..),
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    keyOf,
    selectOr,
    traverseCollection,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLErrors,
    InternalError,
    Message,
    ObjectEntry (..),
    Ref,
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    Token,
    TypeName,
    UnionTag (..),
    VALID,
    ValidValue,
    ValidationErrors,
    Value (..),
    Value (..),
    decodeScientific,
    msg,
    packName,
    toGQLError,
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
    Failure InternalError f,
    Failure InternalError m
  ) =>
  SemigroupM f (ResolverValue m)
  where
  mergeM _ ResNull ResNull = pure ResNull
  mergeM _ ResScalar {} x@ResScalar {} = pure x
  mergeM _ ResEnum {} x@ResEnum {} = pure x
  mergeM p (ResObject x) (ResObject y) = ResObject <$> mergeM p x y
  mergeM _ _ _ = failure ("can't merge: incompatible resolvers" :: InternalError)

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
    Failure InternalError m
  ) =>
  SemigroupM f (ResolverObject m)
  where
  mergeM path (ResolverObject tyname x) (ResolverObject _ y) =
    pure $ ResolverObject tyname (HM.unionWith (mergeResolver path) x y)

mergeResolver ::
  (Monad m, SemigroupM m a) =>
  [Ref FieldName] ->
  m a ->
  m a ->
  m a
mergeResolver path a b = do
  a' <- a
  b >>= mergeM path a'

lookupRes ::
  ( Monad m,
    MonadReader ResolverContext m,
    Failure GQLErrors m,
    Failure ValidationErrors m,
    Failure Message m,
    Failure InternalError m
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
    Failure Message m,
    Failure GQLErrors m,
    Failure ValidationErrors m,
    Failure InternalError m
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
    encodeNode ResEnum {} _ = failure ("wrong selection on enum value" :: Message)
    -- UNION
    encodeNode (ResUnion typename unionRef) (UnionSelection interface selections) = do
      unionRes <- unionRef
      selection <- selectOr (pure interface) ((interface <:>) . unionTagSelection) typename selections
      resolveObject selection unionRes
    encodeNode (ResUnion _ unionRef) (SelectionSet selection) =
      unionRef >>= resolveObject selection
    encodeNode (ResUnion name _) SelectionField =
      failure ("union Resolver " <> msg name <> " cant resolve  SelectionField")
    -- SCALARS
    encodeNode ResNull _ = pure Null
    encodeNode (ResScalar x) SelectionField = pure $ Scalar x
    encodeNode ResScalar {} _ =
      failure ("scalar Resolver should only receive SelectionField" :: Message)

runDataResolver ::
  ( Monad m,
    MonadReader ResolverContext m,
    Failure Message m,
    Failure GQLErrors m,
    Failure ValidationErrors m,
    Failure InternalError m
  ) =>
  ResolverValue m ->
  m ValidValue
runDataResolver res = asks currentSelection >>= __encode res

withObject ::
  ( Monad m,
    Failure GQLErrors m
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
    checkContent _ = failure [toGQLError $ subfieldsNotSelected selectionName "" selectionPosition]

resolveObject ::
  forall m.
  ( Monad m,
    MonadReader ResolverContext m,
    Failure ValidationErrors m,
    Failure InternalError m,
    Failure GQLErrors m,
    Failure Message m
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
resolveObject _ _ = failure ("expected object as resolver" :: InternalError)

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
