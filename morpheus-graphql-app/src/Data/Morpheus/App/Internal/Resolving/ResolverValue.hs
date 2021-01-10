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
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Ext
  ( SemigroupM (..),
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    empty,
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
    TypeName (..),
    UnionSelection,
    UnionTag (..),
    VALID,
    ValidValue,
    ValidationErrors,
    Value (..),
    Value (..),
    msg,
    toGQLError,
    unitFieldName,
    unitTypeName,
  )
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
  [Ref] ->
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
    pure . Scalar . String . readTypeName . __typename
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
    encodeNode (ResEnum enum) SelectionField = pure $ Scalar $ String $ readTypeName enum
    encodeNode (ResEnum name) unionSel@UnionSelection {} =
      encodeNode (mkUnion name mkEnumNull) unionSel
    encodeNode ResEnum {} _ = failure ("wrong selection on enum value" :: Message)
    -- UNION
    encodeNode (ResUnion typename unionRef) (UnionSelection selections) =
      unionRef >>= resolveObject currentSelection
      where
        currentSelection = pickSelection typename selections
    encodeNode (ResUnion name _) _ =
      failure ("union Resolver " <> msg name <> " should only recieve UnionSelection")
    -- SCALARS
    encodeNode ResNull _ = pure Null
    encodeNode (ResScalar x) SelectionField = pure $ Scalar x
    encodeNode ResScalar {} _ =
      failure ("scalar Resolver should only recieve SelectionField" :: Message)

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

pickSelection :: TypeName -> UnionSelection VALID -> SelectionSet VALID
pickSelection = selectOr empty unionTagSelection

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
    checkContent (UnionSelection unionSel) =
      f (selectOr empty unionTagSelection __typename unionSel)
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
