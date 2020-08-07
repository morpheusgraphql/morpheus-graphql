{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.Fragment
  ( validateFragments,
    castFragmentType,
    resolveSpread,
  )
where

import Control.Applicative ((*>), pure)
import Control.Monad ((>>=))
import Data.Foldable (concat, concatMap, traverse_)
import Data.Functor (($>), (<$>), fmap)
import Data.List (elem, lookup)
import Data.Maybe (Maybe (..))
-- MORPHEUS
import Data.Morpheus.Error.Fragment
  ( cannotBeSpreadOnType,
    cannotSpreadWithinItself,
  )
import Data.Morpheus.Internal.Graph (cycleChecking)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    FieldName,
    Fragment (..),
    Fragments,
    Position,
    RAW,
    Ref (..),
    Schema,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName,
    TypeNameRef (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Validation
  ( BaseValidator,
    Constraint (..),
    SelectionValidator,
    askFragments,
    askSchema,
    checkUnused,
    constraint,
    selectKnown,
  )
import Data.Semigroup ((<>))
import Prelude
  ( ($),
    (.),
    otherwise,
  )

validateFragments :: SelectionSet RAW -> BaseValidator ()
validateFragments selectionSet =
  fragmentsCycleChecking
    *> checkUnusedFragments selectionSet
    *> fragmentsConditionTypeChecking

checkUnusedFragments :: SelectionSet RAW -> BaseValidator ()
checkUnusedFragments selectionSet = do
  fragments <- askFragments
  checkUnused
    (usedFragments fragments (elems selectionSet))
    (elems fragments)

castFragmentType ::
  Maybe FieldName ->
  Position ->
  [TypeName] ->
  Fragment RAW ->
  SelectionValidator (Fragment RAW)
castFragmentType key position typeMembers fragment@Fragment {fragmentType}
  | fragmentType `elem` typeMembers = pure fragment
  | otherwise = failure $ cannotBeSpreadOnType key fragmentType position typeMembers

resolveSpread :: [TypeName] -> Ref -> SelectionValidator (Fragment RAW)
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

fragmentsConditionTypeChecking :: BaseValidator ()
fragmentsConditionTypeChecking =
  askFragments
    >>= traverse_ checkTypeExistence

checkTypeExistence :: Fragment RAW -> BaseValidator ()
checkTypeExistence fr@Fragment {fragmentType, fragmentPosition} =
  ( (askSchema :: BaseValidator (Schema VALID))
      >>= selectKnown (TypeNameRef fragmentType fragmentPosition)
      >>= constraint OBJECT fr
  )
    $> ()

fragmentsCycleChecking :: BaseValidator ()
fragmentsCycleChecking = exploreSpreads >>= fragmentCycleChecking

fragmentCycleChecking :: Graph -> BaseValidator ()
fragmentCycleChecking = cycleChecking (failure . cannotSpreadWithinItself)

exploreSpreads :: BaseValidator Graph
exploreSpreads = fmap exploreFragmentSpreads . elems <$> askFragments

exploreFragmentSpreads :: Fragment RAW -> NodeEdges
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

type Node = Ref

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]
