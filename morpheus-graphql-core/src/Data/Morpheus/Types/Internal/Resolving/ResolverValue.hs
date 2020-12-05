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

module Data.Morpheus.Types.Internal.Resolving.ResolverValue
  ( ResModel (..),
    ObjectResModel (..),
    mkUnion,
    mkEnum,
    mkEnumNull,
    resolveObject,
    FieldResModel,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Error.Selection (subfieldsNotSelected)
import Data.Morpheus.Ext.SemigroupM (SemigroupM (..))
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
    GQLValue (..),
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
    msg,
    toGQLError,
    unitFieldName,
    unitTypeName,
  )
import Data.Morpheus.Types.Internal.Resolving.ResolverState
  ( ResolverContext (..),
  )
import Relude hiding (Show, empty)
import Prelude (Show (..))

data ResModel (m :: * -> *)
  = ResNull
  | ResScalar ScalarValue
  | ResEnum TypeName
  | ResObject (ObjectResModel m)
  | ResList [ResModel m]
  | ResUnion TypeName (m (ResModel m))

instance Show (ResModel m) where
  show _ = "TODO:"

instance
  ( Monad f,
    Monad m,
    Failure InternalError f,
    Failure InternalError m
  ) =>
  SemigroupM f (ResModel m)
  where
  mergeM _ ResNull ResNull = pure ResNull
  mergeM _ ResScalar {} x@ResScalar {} = pure x
  mergeM _ ResEnum {} x@ResEnum {} = pure x
  mergeM p (ResObject x) (ResObject y) = ResObject <$> mergeM p x y
  mergeM _ _ _ = failure ("can't merge: incompatible resolvers" :: InternalError)

type FieldResModel m = (FieldName, m (ResModel m))

data ObjectResModel m = ObjectResModel
  { __typename :: TypeName,
    objectFields :: HashMap FieldName (m (ResModel m))
  }

instance Show (ObjectResModel m) where
  show _ = "TODO:"

instance
  ( Monad m,
    Applicative f,
    Failure InternalError m
  ) =>
  SemigroupM f (ObjectResModel m)
  where
  mergeM path (ObjectResModel tyname x) (ObjectResModel _ y) =
    pure $ ObjectResModel tyname (HM.unionWith (mergeResolver path) x y)

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
  ObjectResModel m ->
  m ValidValue
lookupRes Selection {selectionName}
  | selectionName == "__typename" =
    pure . Scalar . String . readTypeName . __typename
  | otherwise =
    maybe
      (pure gqlNull)
      (>>= runDataResolver)
      . HM.lookup selectionName
      . objectFields

mkUnion ::
  (Monad m) =>
  TypeName ->
  [FieldResModel m] ->
  ResModel m
mkUnion name =
  ResUnion
    name
    . pure
    . mkObject
      name

mkEnum :: TypeName -> ResModel m
mkEnum = ResEnum

mkEnumNull :: (Monad m) => [FieldResModel m]
mkEnumNull = [(unitFieldName, pure $ mkEnum unitTypeName)]

mkObject ::
  TypeName ->
  [FieldResModel m] ->
  ResModel m
mkObject __typename fields =
  ResObject
    ( ObjectResModel
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
  ResModel m ->
  Selection VALID ->
  m (Value VALID)
__encode obj sel@Selection {selectionContent} = encodeNode obj selectionContent
  where
    -- LIST
    encodeNode ::
      ResModel m ->
      SelectionContent VALID ->
      m (Value VALID)
    encodeNode (ResList x) _ = List <$> traverse runDataResolver x
    -- Object -----------------
    encodeNode objDrv@(ResObject ObjectResModel {__typename}) _ = withObject __typename (`resolveObject` objDrv) sel
    -- ENUM
    encodeNode (ResEnum enum) SelectionField = pure $ gqlString $ readTypeName enum
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
  ResModel m ->
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
  ResModel m ->
  m ValidValue
resolveObject selectionSet (ResObject drv@ObjectResModel {__typename}) =
  Object <$> traverseCollection resolver selectionSet
  where
    resolver :: Selection VALID -> m (ObjectEntry VALID)
    resolver currentSelection =
      local (\ctx -> ctx {currentSelection, currentTypeName = __typename}) $
        ObjectEntry (keyOf currentSelection) <$> lookupRes currentSelection drv
resolveObject _ _ = failure ("expected object as resolver" :: InternalError)
