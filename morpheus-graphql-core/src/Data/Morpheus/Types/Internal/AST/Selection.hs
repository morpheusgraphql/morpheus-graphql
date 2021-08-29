{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Selection
  ( Selection (..),
    SelectionContent (..),
    SelectionSet,
    UnionTag (..),
    UnionSelection,
    Fragment (..),
    Fragments,
    Operation (..),
    Variable (..),
    VariableDefinitions,
    DefaultValue,
    getOperationName,
    getOperationDataType,
    splitSystemSelection,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (foldr')
import Data.Mergeable
  ( Merge (..),
    MergeMap,
    NameCollision (..),
    OrdMap,
  )
import Data.Mergeable.MergeMap (partition)
import Data.Morpheus.Error.Operation
  ( mutationIsNotDefined,
    subscriptionIsNotDefined,
  )
import Data.Morpheus.Internal.Utils
  ( HistoryT,
    KeyOf (..),
    addPath,
    (<:>),
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    newline,
    renderObject,
    space,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Position,
    Ref (..),
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError,
    at,
    atPositions,
    msg,
  )
import Data.Morpheus.Types.Internal.AST.Fields
  ( Arguments,
    Directives,
    renderArgumentValues,
    renderDirectives,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
    FragmentName,
    TypeName,
    intercalate,
    isNotSystemFieldName,
  )
import Data.Morpheus.Types.Internal.AST.OperationType (OperationType (..))
import Data.Morpheus.Types.Internal.AST.Stage
  ( ALLOW_DUPLICATES,
    RAW,
    Stage,
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( OBJECT,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( Schema (..),
    TypeDefinition (..),
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ResolvedValue,
    Variable (..),
    VariableDefinitions,
  )
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding (intercalate, show)
import Prelude (show)

data Fragment (stage :: Stage) = Fragment
  { fragmentName :: FragmentName,
    fragmentType :: TypeName,
    fragmentPosition :: Position,
    fragmentSelection :: SelectionSet stage,
    fragmentDirectives :: Directives stage
  }
  deriving (Show, Eq, Lift)

-- ERRORs
instance NameCollision GQLError (Fragment s) where
  nameCollision Fragment {fragmentName, fragmentPosition} =
    ("There can be only one fragment named " <> msg fragmentName <> ".")
      `at` fragmentPosition

instance KeyOf FragmentName (Fragment s) where
  keyOf = fragmentName

type Fragments (s :: Stage) = OrdMap FragmentName (Fragment s)

data SelectionContent (s :: Stage) where
  SelectionField :: SelectionContent s
  SelectionSet :: SelectionSet s -> SelectionContent s
  UnionSelection ::
    { defaultSelection :: SelectionSet VALID,
      conditionalSelections :: UnionSelection VALID
    } ->
    SelectionContent VALID

renderSelectionSet :: SelectionSet VALID -> Rendering
renderSelectionSet = renderObject . toList

instance RenderGQL (SelectionContent VALID) where
  renderGQL SelectionField = ""
  renderGQL (SelectionSet selSet) = renderSelectionSet selSet
  renderGQL (UnionSelection interfaceFields unionSets) =
    renderObject unionSelectionElements
    where
      unionSelectionElements :: [Either (Selection VALID) UnionTag]
      unionSelectionElements =
        map Left (toList interfaceFields)
          <> map Right (toList unionSets)

instance
  ( Monad m,
    MonadError GQLError m,
    Merge (HistoryT m) (SelectionSet s)
  ) =>
  Merge (HistoryT m) (SelectionContent s)
  where
  merge (SelectionSet s1) (SelectionSet s2) = SelectionSet <$> merge s1 s2
  merge (UnionSelection m1 u1) (UnionSelection m2 u2) = UnionSelection <$> merge m1 m2 <*> merge u1 u2
  merge oldC currentC
    | oldC == currentC = pure oldC
    | otherwise = do
      path <- ask
      throwError
        ( msg (intercalate "." $ fmap refName path)
            `atPositions` fmap refPosition path
        )

deriving instance Show (SelectionContent a)

deriving instance Eq (SelectionContent a)

deriving instance Lift (SelectionContent a)

data UnionTag = UnionTag
  { unionTagName :: TypeName,
    unionTagSelection :: SelectionSet VALID
  }
  deriving (Show, Eq, Lift)

instance KeyOf TypeName UnionTag where
  keyOf = unionTagName

instance RenderGQL UnionTag where
  renderGQL UnionTag {unionTagName, unionTagSelection} =
    "... on "
      <> renderGQL unionTagName
      <> renderSelectionSet unionTagSelection

mergeConflict :: (Monad m, MonadError GQLError m) => GQLError -> HistoryT m a
mergeConflict err = do
  path <- ask
  __mergeConflict path
  where
    __mergeConflict :: (Monad m, MonadError GQLError m) => [Ref FieldName] -> HistoryT m a
    __mergeConflict [] = throwError err
    __mergeConflict refs@(rootField : xs) =
      throwError
        (renderSubfields `atPositions` fmap refPosition refs <> err)
      where
        fieldConflicts ref = msg (refName ref) <> " conflict because "
        renderSubfield ref txt = txt <> "subfields " <> fieldConflicts ref
        renderStart = "Fields " <> fieldConflicts rootField
        renderSubfields =
          foldr'
            renderSubfield
            renderStart
            xs

instance
  ( Monad m,
    MonadError GQLError m
  ) =>
  Merge (HistoryT m) UnionTag
  where
  merge (UnionTag oldTag oldSel) (UnionTag _ currentSel) =
    UnionTag oldTag <$> merge oldSel currentSel

type UnionSelection (s :: Stage) = MergeMap (ALLOW_DUPLICATES s) TypeName UnionTag

type SelectionSet (s :: Stage) = MergeMap (ALLOW_DUPLICATES s) FieldName (Selection s)

splitSystemSelection :: SelectionSet s -> (Maybe (SelectionSet s), Maybe (SelectionSet s))
splitSystemSelection = partition (not . isNotSystemFieldName . selectionName)

data Selection (s :: Stage) where
  Selection ::
    { selectionPosition :: Position,
      selectionAlias :: Maybe FieldName,
      selectionName :: FieldName,
      selectionArguments :: Arguments s,
      selectionDirectives :: Directives s,
      selectionContent :: SelectionContent s
    } ->
    Selection s
  InlineFragment :: Fragment RAW -> Selection RAW
  Spread :: Directives RAW -> Ref FragmentName -> Selection RAW

instance RenderGQL (Selection VALID) where
  renderGQL
    Selection
      { ..
      } =
      renderGQL (fromMaybe selectionName selectionAlias)
        <> renderArgumentValues selectionArguments
        <> renderDirectives selectionDirectives
        <> renderGQL selectionContent

instance KeyOf FieldName (Selection s) where
  keyOf
    Selection
      { selectionName,
        selectionAlias
      } = fromMaybe selectionName selectionAlias
  keyOf _ = ""

useDifferentAliases :: GQLError
useDifferentAliases =
  "Use different aliases on the "
    <> "fields to fetch both if this was intentional."

instance
  ( Monad m,
    MonadError GQLError m,
    Merge (HistoryT m) (SelectionSet s)
  ) =>
  Merge (HistoryT m) (Selection s)
  where
  merge = mergeSelection

mergeSelection ::
  ( Monad m,
    MonadError GQLError m,
    Merge (HistoryT m) (SelectionSet s)
  ) =>
  Selection s ->
  Selection s ->
  HistoryT m (Selection s)
mergeSelection
  old@Selection {selectionPosition = pos1}
  current@Selection {selectionPosition = pos2} =
    do
      selectionName <- mergeName [pos1, pos2] old current
      addPath (Ref selectionName pos1) $ do
        selectionArguments <- mergeArguments
        selectionContent <- merge (selectionContent old) (selectionContent current)
        dirs <- selectionDirectives old <:> selectionDirectives current
        pure $
          Selection
            { selectionAlias = mergeAlias,
              selectionPosition = pos1,
              selectionDirectives = dirs,
              ..
            }
    where
      mergeAlias
        | all (isJust . selectionAlias) [old, current] = selectionAlias old
        | otherwise = Nothing
      --- arguments must be equal
      mergeArguments
        | selectionArguments old == selectionArguments current = pure $ selectionArguments current
        | otherwise =
          mergeConflict $
            ("they have differing arguments. " <> useDifferentAliases)
              `atPositions` [pos1, pos2]
mergeSelection x y = mergeConflict ("INTERNAL: can't merge. " <> msgValue x <> msgValue y <> useDifferentAliases)

msgValue :: Show a => a -> GQLError
msgValue = msg . show

-- fails if alias matches but name not:
--   { user1: user
--     user1: product
--   }
mergeName ::
  (Monad m, MonadError GQLError m, Foldable t) =>
  t Position ->
  Selection s1 ->
  Selection s2 ->
  HistoryT m FieldName
mergeName pos old current
  | selectionName old == selectionName current = pure $ selectionName current
  | otherwise =
    mergeConflict $
      ( msg (selectionName old)
          <> " and "
          <> msg (selectionName current)
          <> " are different fields. "
          <> useDifferentAliases
      )
        `atPositions` pos

deriving instance Show (Selection a)

deriving instance Lift (Selection a)

deriving instance Eq (Selection a)

type DefaultValue = Maybe ResolvedValue

data Operation (s :: Stage) = Operation
  { operationPosition :: Position,
    operationType :: OperationType,
    operationName :: Maybe FieldName,
    operationArguments :: VariableDefinitions s,
    operationDirectives :: Directives s,
    operationSelection :: SelectionSet s
  }
  deriving (Show, Lift)

instance RenderGQL (Operation VALID) where
  renderGQL
    Operation
      { operationName,
        operationType,
        operationDirectives,
        operationSelection
      } =
      renderGQL operationType
        <> maybe "" ((space <>) . renderGQL) operationName
        <> renderDirectives operationDirectives
        <> renderSelectionSet operationSelection
        <> newline

getOperationName :: Maybe FieldName -> TypeName
getOperationName = maybe "AnonymousOperation" coerce

getOperationDataType :: MonadError GQLError m => Operation s -> Schema VALID -> m (TypeDefinition OBJECT VALID)
getOperationDataType Operation {operationType = Query} lib = pure (query lib)
getOperationDataType Operation {operationType = Mutation, operationPosition} lib =
  maybe (throwError $ mutationIsNotDefined operationPosition) pure (mutation lib)
getOperationDataType Operation {operationType = Subscription, operationPosition} lib =
  maybe (throwError $ subscriptionIsNotDefined operationPosition) pure (subscription lib)
