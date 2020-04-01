{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Fragment
  ( validateFragments
  , castFragmentType
  , resolveSpread
  )
where

import           Data.List                      ( (\\) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )

-- MORPHEUS
import           Data.Morpheus.Error.Fragment   ( cannotBeSpreadOnType
                                                , cannotSpreadWithinItself
                                                , unusedFragment
                                                )
import           Data.Morpheus.Error.Variable   ( unknownType )
import           Data.Morpheus.Types.Internal.AST
                                                ( Fragment(..)
                                                , Fragments
                                                , SelectionContent(..)
                                                , Selection(..)
                                                , Ref(..)
                                                , Position
                                                , Schema
                                                , SelectionSet
                                                , RAW
                                                , selectTypeObject
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( selectOr
                                                , toList
                                                , toAssoc
                                                , selectKnown
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )


validateFragments
  :: Schema -> Fragments -> SelectionSet RAW -> Validation ()
validateFragments lib fragments operatorSel =checkLoop >> checkUnusedFragments
 where
  checkUnusedFragments =
    case fragmentsKeys \\ usedFragments fragments (toAssoc operatorSel) of
      []     -> return ()
      unused -> failure (unusedFragment unused)
  checkLoop = traverse (validateFragment lib) (toList fragments) >>= detectLoopOnFragments
  fragmentsKeys = map toRef (toList fragments)
    where toRef Fragment { fragmentName , fragmentPosition } = Ref fragmentName fragmentPosition

type Node = Ref

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

castFragmentType
  :: Maybe Text -> Position -> [Text] -> Fragment -> Validation Fragment
castFragmentType key' position' typeMembers fragment@Fragment { fragmentType }
  = if fragmentType `elem` typeMembers
    then pure fragment
    else failure $ cannotBeSpreadOnType key' fragmentType position' typeMembers

resolveSpread :: Fragments -> [Text] -> Ref -> Validation Fragment
resolveSpread fragments allowedTargets reference@Ref { refName, refPosition } =
  selectKnown reference fragments
    >>= castFragmentType (Just refName) refPosition allowedTargets

usedFragments :: Fragments -> [(Text, Selection RAW)] -> [Node]
usedFragments fragments = concatMap (findAllUses . snd)
 where
  findAllUses :: Selection RAW -> [Node]
  findAllUses Selection { selectionContent = SelectionField } = []
  findAllUses Selection { selectionContent = SelectionSet selectionSet } =
    concatMap findAllUses selectionSet
  findAllUses (InlineFragment Fragment { fragmentSelection }) =
    concatMap findAllUses fragmentSelection
  findAllUses (Spread Ref { refName, refPosition }) =
    [Ref refName refPosition] <> searchInFragment
   where
    searchInFragment = selectOr 
      [] 
      (concatMap findAllUses . fragmentSelection) 
      refName 
      fragments

scanForSpread :: Selection RAW -> [Node]
scanForSpread Selection { selectionContent = SelectionField } = []
scanForSpread Selection { selectionContent = SelectionSet selectionSet } =
  concatMap scanForSpread selectionSet
scanForSpread (InlineFragment Fragment { fragmentSelection }) =
  concatMap scanForSpread fragmentSelection
scanForSpread (Spread Ref { refName, refPosition }) =
  [Ref refName refPosition]

validateFragment :: Schema -> Fragment -> Validation NodeEdges
validateFragment lib  Fragment { fragmentName, fragmentSelection, fragmentType, fragmentPosition }
  = selectTypeObject validationError fragmentType lib >> pure
    (Ref fragmentName fragmentPosition, concatMap scanForSpread fragmentSelection)
  where validationError = unknownType fragmentType fragmentPosition

detectLoopOnFragments :: Graph -> Validation ()
detectLoopOnFragments lib = mapM_ checkFragment lib
 where
  checkFragment (fragmentID, _) = checkForCycle lib fragmentID [fragmentID]

checkForCycle :: Graph -> Node -> [Node] -> Validation Graph
checkForCycle lib parentNode history = case lookup parentNode lib of
  Just node -> concat <$> mapM checkNode node
  Nothing   -> pure []
 where
  checkNode x = if x `elem` history then cycleError x else recurse x
  recurse node = checkForCycle lib node $ history ++ [node]
  cycleError n = failure $ cannotSpreadWithinItself (n : history)
