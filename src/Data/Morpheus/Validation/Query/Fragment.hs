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
import qualified Data.Text                     as T
                                                ( concat )

-- MORPHEUS
import           Data.Morpheus.Error.Fragment   ( cannotBeSpreadOnType
                                                , cannotSpreadWithinItself
                                                , fragmentNameCollision
                                                , unknownFragment
                                                , unusedFragment
                                                )
import           Data.Morpheus.Error.Variable   ( unknownType )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Fragment(..)
                                                , FragmentLib
                                                , RawSelection(..)
                                                , Selection(..)
                                                )
import           Data.Morpheus.Types.Internal.Base
                                                ( Reference(..)
                                                , Position
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataTypeLib
                                                , lookupDataObject
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation )
import           Data.Morpheus.Validation.Internal.Utils
                                                ( checkNameCollision )


validateFragments
  :: DataTypeLib -> FragmentLib -> [(Text, RawSelection)] -> Validation ()
validateFragments lib fragments operatorSel =
  validateNameCollision >> checkLoop >> checkUnusedFragments
 where
  validateNameCollision =
    checkNameCollision fragmentsKeys fragmentNameCollision
  checkUnusedFragments =
    case fragmentsKeys \\ usedFragments fragments operatorSel of
      []     -> return ()
      unused -> Left $ unusedFragment unused
  checkLoop = mapM (validateFragment lib) fragments >>= detectLoopOnFragments
  fragmentsKeys = map toReference fragments
   where
    toReference (key, Fragment { fragmentPosition }) =
      Reference key fragmentPosition

type Node = Reference

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

getFragment :: Reference -> FragmentLib -> Validation Fragment
getFragment Reference { refName, refPosition } lib = case lookup refName lib of
  Nothing       -> Left $ unknownFragment refName refPosition
  Just fragment -> pure fragment

castFragmentType
  :: Maybe Text -> Position -> [Text] -> Fragment -> Validation Fragment
castFragmentType key' position' targets' fragment@Fragment { fragmentType } =
  if fragmentType `elem` targets'
    then pure fragment
    else Left
      $ cannotBeSpreadOnType key' fragmentType position' (T.concat targets')

resolveSpread :: FragmentLib -> [Text] -> Reference -> Validation Fragment
resolveSpread fragments allowedTargets reference@Reference { refName, refPosition }
  = getFragment reference fragments
    >>= castFragmentType (Just refName) refPosition allowedTargets

usedFragments :: FragmentLib -> [(Text, RawSelection)] -> [Node]
usedFragments fragments = concatMap findAllUses
 where
  findAllUses :: (Text, RawSelection) -> [Node]
  findAllUses (_, RawSelectionSet Selection { selectionRec }) =
    concatMap findAllUses selectionRec
  findAllUses (_, InlineFragment Fragment { fragmentSelection }) =
    concatMap findAllUses fragmentSelection
  findAllUses (_, RawSelectionField{}) = []
  findAllUses (_, Spread Reference { refName, refPosition }) =
    [Reference refName refPosition] <> searchInFragment
   where
    searchInFragment = maybe []
                             (concatMap findAllUses . fragmentSelection)
                             (lookup refName fragments)

scanForSpread :: (Text, RawSelection) -> [Node]
scanForSpread (_, RawSelectionSet Selection { selectionRec }) =
  concatMap scanForSpread selectionRec
scanForSpread (_, InlineFragment Fragment { fragmentSelection = selection' }) =
  concatMap scanForSpread selection'
scanForSpread (_, RawSelectionField{}) = []
scanForSpread (_, Spread Reference { refName = name', refPosition = position' })
  = [Reference name' position']

validateFragment :: DataTypeLib -> (Text, Fragment) -> Validation NodeEdges
validateFragment lib (fName, Fragment { fragmentSelection, fragmentType, fragmentPosition })
  = lookupDataObject validationError fragmentType lib >> pure
    ( Reference fName fragmentPosition
    , concatMap scanForSpread fragmentSelection
    )
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
  cycleError n = Left $ cannotSpreadWithinItself $ history ++ [n]
