{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.Fragment
  ( validateFragments,
    castFragmentType,
    resolveSpread,
    validateFragment,
    resolveValidFragment,
    selectFragmentType,
  )
where

import Control.Applicative ((*>), (<*>), pure)
import Control.Monad ((>>=))
import Data.Foldable (concatMap, traverse_)
import Data.Functor (($>), (<$>), fmap)
import Data.List (elem)
import Data.Maybe (Maybe (..))
-- MORPHEUS
import Data.Morpheus.Error.Fragment
  ( cannotBeSpreadOnType,
    cannotSpreadWithinItself,
  )
import Data.Morpheus.Internal.Graph
  ( Edges,
    Graph,
    Node,
    cycleChecking,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( Directives,
    FieldName,
    FieldsDefinition,
    Fragment (..),
    Fragments,
    OUT,
    Position,
    RAW,
    Ref (..),
    Schema,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeDefinition,
    TypeName,
    TypeNameRef (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Validation
  ( Constraint (..),
    FragmentValidator,
    SelectionValidator,
    askFragments,
    askSchema,
    checkUnused,
    constraint,
    selectKnown,
  )
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (.),
    otherwise,
  )

resolveValidFragment ::
  [TypeName] ->
  Ref ->
  FragmentValidator s (SelectionSet s)
resolveValidFragment allowedTargets ref =
  fragmentSelection <$> resolveSpread allowedTargets ref

validateFragment ::
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  [TypeName] ->
  Fragment RAW ->
  FragmentValidator s (Fragment VALID)
validateFragment validate allowedTypes fragment@Fragment {fragmentPosition} =
  castFragmentType Nothing fragmentPosition allowedTypes fragment
    >>= onlyValidateFrag validate

validateFragments ::
  (Fragment RAW -> FragmentValidator RAW (SelectionSet VALID)) ->
  SelectionSet RAW ->
  FragmentValidator RAW (Fragments VALID)
validateFragments f selectionSet =
  fragmentsCycleChecking
    *> checkUnusedFragments selectionSet
    *> fragmentsConditionTypeChecking
    *> __validateFragments f

__validateFragments ::
  (Fragment RAW -> FragmentValidator RAW (SelectionSet VALID)) ->
  FragmentValidator RAW (Fragments VALID)
__validateFragments f = askFragments >>= traverse (onlyValidateFrag f)

onlyValidateFrag ::
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  Fragment RAW ->
  FragmentValidator s (Fragment VALID)
onlyValidateFrag validate f@Fragment {..} =
  Fragment
    fragmentName
    fragmentType
    fragmentPosition
    <$> validate f <*> validateFragmentDirectives fragmentDirectives

validateFragmentDirectives :: Directives RAW -> FragmentValidator s (Directives VALID)
validateFragmentDirectives _ = pure []

checkUnusedFragments :: SelectionSet RAW -> FragmentValidator RAW ()
checkUnusedFragments selectionSet = do
  fragments <- askFragments
  checkUnused
    (usedFragments fragments (elems selectionSet))
    (elems fragments)

castFragmentType ::
  Maybe FieldName ->
  Position ->
  [TypeName] ->
  Fragment s ->
  FragmentValidator s1 (Fragment s)
castFragmentType key position typeMembers fragment@Fragment {fragmentType}
  | fragmentType `elem` typeMembers = pure fragment
  | otherwise = failure $ cannotBeSpreadOnType key fragmentType position typeMembers

resolveSpread :: [TypeName] -> Ref -> FragmentValidator s (Fragment s)
resolveSpread allowedTargets ref@Ref {refName, refPosition} =
  askFragments
    >>= selectKnown ref
    >>= castFragmentType (Just refName) refPosition allowedTargets

usedFragments :: Fragments RAW -> [Selection RAW] -> [Node]
usedFragments fragments = concatMap findAllUses
  where
    findUsesSelectionContent :: SelectionContent RAW -> [Node]
    findUsesSelectionContent (SelectionSet selectionSet) =
      concatMap findAllUses selectionSet
    findUsesSelectionContent SelectionField = []
    findAllUses :: Selection RAW -> [Node]
    findAllUses Selection {selectionContent} =
      findUsesSelectionContent selectionContent
    findAllUses (InlineFragment Fragment {fragmentSelection}) =
      concatMap findAllUses fragmentSelection
    findAllUses (Spread _ Ref {refName, refPosition}) =
      [Ref refName refPosition] <> searchInFragment
      where
        searchInFragment =
          selectOr
            []
            (concatMap findAllUses . fragmentSelection)
            refName
            fragments

fragmentsConditionTypeChecking :: FragmentValidator RAW ()
fragmentsConditionTypeChecking =
  askFragments
    >>= traverse_ checkTypeExistence

checkTypeExistence :: Fragment RAW -> FragmentValidator RAW ()
checkTypeExistence fr = selectFragmentType fr $> ()

selectFragmentType :: Fragment RAW -> FragmentValidator s (TypeDefinition OUT VALID, FieldsDefinition OUT VALID)
selectFragmentType fr@Fragment {fragmentType, fragmentPosition} = do
  (schema :: Schema VALID) <- askSchema
  typeDef <- selectKnown (TypeNameRef fragmentType fragmentPosition) schema
  constraint OBJECT fr typeDef

fragmentsCycleChecking :: FragmentValidator RAW ()
fragmentsCycleChecking =
  exploreSpreads
    >>= cycleChecking (failure . cannotSpreadWithinItself)

exploreSpreads :: FragmentValidator RAW Graph
exploreSpreads = fmap exploreFragmentSpreads . elems <$> askFragments

exploreFragmentSpreads :: Fragment RAW -> Edges
exploreFragmentSpreads Fragment {fragmentName, fragmentSelection, fragmentPosition} =
  (Ref fragmentName fragmentPosition, concatMap scanSpread fragmentSelection)

class ScanSpread a where
  scanSpread :: a -> [Node]

instance ScanSpread (Selection RAW) where
  scanSpread Selection {selectionContent} =
    scanSpread selectionContent
  scanSpread (InlineFragment Fragment {fragmentSelection}) =
    concatMap scanSpread fragmentSelection
  scanSpread (Spread _ Ref {refName, refPosition}) =
    [Ref refName refPosition]

instance ScanSpread (SelectionContent RAW) where
  scanSpread SelectionField = []
  scanSpread (SelectionSet selectionSet) =
    concatMap scanSpread selectionSet
