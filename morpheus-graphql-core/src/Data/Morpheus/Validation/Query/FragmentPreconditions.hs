{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.FragmentPreconditions
  ( checkFragmentPreconditions,
  )
where

import Control.Applicative ((*>))
import Control.Monad ((>>=))
import Data.Foldable (concatMap)
import Data.Functor ((<$>), fmap)
-- MORPHEUS
import Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself,
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
  ( Fragment (..),
    Fragments,
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
  )
import Data.Morpheus.Types.Internal.Validation
  ( BaseValidator,
    askFragments,
    checkUnused,
  )
import Data.Semigroup ((<>))
import Prelude
  ( (.),
  )

checkUnusedFragments :: SelectionSet RAW -> BaseValidator ()
checkUnusedFragments selectionSet = do
  fragments <- askFragments
  checkUnused
    (usedFragments fragments (elems selectionSet))
    (elems fragments)

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

checkFragmentPreconditions :: SelectionSet RAW -> BaseValidator ()
checkFragmentPreconditions selection =
  (exploreSpreads >>= cycleChecking (failure . cannotSpreadWithinItself))
    *> checkUnusedFragments selection

exploreSpreads :: BaseValidator Graph
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
