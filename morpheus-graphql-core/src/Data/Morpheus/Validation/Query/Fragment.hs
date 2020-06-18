{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Fragment
  ( validateFragments,
    castFragmentType,
    resolveSpread,
  )
where

import Data.Foldable (traverse_)
-- MORPHEUS
import Data.Morpheus.Error.Fragment
  ( cannotBeSpreadOnType,
    cannotSpreadWithinItself,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    elems,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    Fragment (..),
    Fragments,
    Position,
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName,
    TypeNameRef (..),
  )
import Data.Morpheus.Types.Internal.Validation
  ( BaseValidator,
    Constraint (..),
    OperationContext,
    Validator,
    askFragments,
    askSchema,
    checkUnused,
    constraint,
    selectKnown,
  )
import Data.Semigroup ((<>))

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
  Maybe FieldName -> Position -> [TypeName] -> Fragment -> Validator ctx Fragment
castFragmentType key position typeMembers fragment@Fragment {fragmentType}
  | fragmentType `elem` typeMembers = pure fragment
  | otherwise = failure $ cannotBeSpreadOnType key fragmentType position typeMembers

resolveSpread :: [TypeName] -> Ref -> Validator (OperationContext v i) Fragment
resolveSpread allowedTargets ref@Ref {refName, refPosition} =
  askFragments
    >>= selectKnown ref
    >>= castFragmentType (Just refName) refPosition allowedTargets

usedFragments :: Fragments -> [Selection RAW] -> [Node]
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
  elems <$> askFragments
    >>= traverse_ checkTypeExistence

checkTypeExistence :: Fragment -> BaseValidator ()
checkTypeExistence fr@Fragment {fragmentType, fragmentPosition} =
  askSchema
    >>= selectKnown (TypeNameRef fragmentType fragmentPosition)
    >>= constraint OBJECT fr
    >> pure ()

fragmentsCycleChecking :: BaseValidator ()
fragmentsCycleChecking = exploreSpreads >>= fragmentCycleChecking

exploreSpreads :: BaseValidator Graph
exploreSpreads = map exploreFragmentSpreads . elems <$> askFragments

exploreFragmentSpreads :: Fragment -> NodeEdges
exploreFragmentSpreads Fragment {fragmentName, fragmentSelection, fragmentPosition} =
  (Ref fragmentName fragmentPosition, concatMap scanForSpread fragmentSelection)

scanForSpreadContent :: SelectionContent RAW -> [Node]
scanForSpreadContent SelectionField = []
scanForSpreadContent (SelectionSet selectionSet) =
  concatMap scanForSpread selectionSet

scanForSpread :: Selection RAW -> [Node]
scanForSpread Selection {selectionContent} =
  scanForSpreadContent selectionContent
scanForSpread (InlineFragment Fragment {fragmentSelection}) =
  concatMap scanForSpread fragmentSelection
scanForSpread (Spread _ Ref {refName, refPosition}) =
  [Ref refName refPosition]

type Node = Ref

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

fragmentCycleChecking :: Graph -> BaseValidator ()
fragmentCycleChecking lib = traverse_ checkFragment lib
  where
    checkFragment (fragmentID, _) = checkForCycle lib fragmentID [fragmentID]

checkForCycle :: Graph -> Node -> [Node] -> BaseValidator Graph
checkForCycle lib parentNode history = case lookup parentNode lib of
  Just node -> concat <$> traverse checkNode node
  Nothing -> pure []
  where
    checkNode x = if x `elem` history then cycleError x else recurse x
    recurse node = checkForCycle lib node $ history ++ [node]
    cycleError n = failure $ cannotSpreadWithinItself (n : history)
