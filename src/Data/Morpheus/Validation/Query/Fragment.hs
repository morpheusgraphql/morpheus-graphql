{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Fragment
  ( validateFragments
  , castFragmentType
  , resolveSpread
  , getFragment
  )
where

import           Data.List                      ( (\\) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )

-- MORPHEUS
import           Data.Morpheus.Error.Fragment   ( cannotBeSpreadOnType
                                                , cannotSpreadWithinItself
                                                , fragmentNameCollision
                                                , unknownFragment
                                                , unusedFragment
                                                )
import           Data.Morpheus.Error.Variable   ( unknownType )
import           Data.Morpheus.Types.Internal.AST
                                                ( Fragment(..)
                                                , FragmentLib
                                                , RawSelection
                                                , SelectionContent(..)
                                                , Selection(..)
                                                , Ref(..)
                                                , Position
                                                , Schema
                                                , DataLookup(..)
                                                , checkNameCollision
                                                , FieldsDefinition
                                                , Name
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )


validateFragments
  :: Schema -> FragmentLib -> [(Text, RawSelection)] -> Validation ()
validateFragments lib fragments operatorSel =
  validateNameCollision >> checkLoop >> checkUnusedFragments
 where
  validateNameCollision =
    checkNameCollision fragmentsKeys fragmentNameCollision
  checkUnusedFragments =
    case fragmentsKeys \\ usedFragments fragments operatorSel of
      []     -> return ()
      unused -> failure (unusedFragment unused)
  checkLoop = mapM (validateFragment lib) fragments >>= detectLoopOnFragments
  fragmentsKeys = map toRef fragments
    where toRef (key, Fragment { fragmentPosition }) = Ref key fragmentPosition

type Node = Ref

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

getFragment :: Ref -> FragmentLib -> Validation Fragment
getFragment Ref { refName, refPosition } lib = case lookup refName lib of
  Nothing       -> failure $ unknownFragment refName refPosition
  Just fragment -> pure fragment

castFragmentType
  :: Maybe Text -> Position -> [Text] -> Fragment -> Validation Fragment
castFragmentType key' position' typeMembers fragment@Fragment { fragmentType }
  = if fragmentType `elem` typeMembers
    then pure fragment
    else failure $ cannotBeSpreadOnType key' fragmentType position' typeMembers

resolveSpread :: FragmentLib -> [Text] -> Ref -> Validation Fragment
resolveSpread fragments allowedTargets reference@Ref { refName, refPosition } =
  getFragment reference fragments
    >>= castFragmentType (Just refName) refPosition allowedTargets

usedFragments :: FragmentLib -> [(Text, RawSelection)] -> [Node]
usedFragments fragments = concatMap (findAllUses . snd)
 where
  findAllUses :: RawSelection -> [Node]
  findAllUses Selection { selectionContent = SelectionField } = []
  findAllUses Selection { selectionContent = SelectionSet selectionSet } =
    concatMap (findAllUses . snd) selectionSet
  findAllUses (InlineFragment Fragment { fragmentSelection }) =
    concatMap (findAllUses . snd) fragmentSelection
  findAllUses (Spread Ref { refName, refPosition }) =
    [Ref refName refPosition] <> searchInFragment
   where
    searchInFragment = maybe
      []
      (concatMap (findAllUses . snd) . fragmentSelection)
      (lookup refName fragments)

scanForSpread :: (Text, RawSelection) -> [Node]
scanForSpread (_, Selection { selectionContent = SelectionField }) = []
scanForSpread (_, Selection { selectionContent = SelectionSet selectionSet }) =
  concatMap scanForSpread selectionSet
scanForSpread (_, InlineFragment Fragment { fragmentSelection = selection' }) =
  concatMap scanForSpread selection'
scanForSpread (_, Spread Ref { refName = name', refPosition = position' }) =
  [Ref name' position']

validateFragment :: Schema -> (Text, Fragment) -> Validation NodeEdges
validateFragment lib (fName, Fragment { fragmentSelection, fragmentType, fragmentPosition })
  = (lookupResult validationError fragmentType lib :: Validation ( Name, FieldsDefinition) )>> pure
    (Ref fName fragmentPosition, concatMap scanForSpread fragmentSelection)
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
