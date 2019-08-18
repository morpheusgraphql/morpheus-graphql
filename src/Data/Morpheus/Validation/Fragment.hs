{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Fragment
  ( validateFragments
  , castFragmentType
  , resolveSpread
  , getFragment
  ) where

import           Data.List                                     ((\\))
import           Data.Semigroup                                ((<>))
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T (concat)

-- MORPHEUS
import           Data.Morpheus.Error.Fragment                  (cannotBeSpreadOnType, cannotSpreadWithinItself,
                                                                fragmentNameCollision, unknownFragment, unusedFragment)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), FragmentLib, RawSelection (..),
                                                                RawSelection' (..), Reference (..))
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Data             (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Validation.Utils.Utils          (checkNameCollision, existsObjectType)

validateFragments ::
     DataTypeLib -> FragmentLib -> [(Text, RawSelection)] -> Validation ()
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
    fragmentsKeys = map toEnhancedKey fragments
      where
        toEnhancedKey (key, Fragment {fragmentPosition}) =
          EnhancedKey key fragmentPosition

type Node = EnhancedKey

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

getFragment :: Reference -> FragmentLib -> Validation Fragment
getFragment Reference {referenceName, referencePosition} lib =
  case lookup referenceName lib of
    Nothing       -> Left $ unknownFragment referenceName referencePosition
    Just fragment -> pure fragment

castFragmentType ::
     Maybe Text -> Position -> [Text] -> Fragment -> Validation Fragment
castFragmentType key' position' targets' fragment@Fragment {fragmentType} =
  if fragmentType `elem` targets'
    then pure fragment
    else Left $
         cannotBeSpreadOnType key' fragmentType position' (T.concat targets')

resolveSpread :: FragmentLib -> [Text] -> Reference -> Validation Fragment
resolveSpread fragments allowedTargets reference@Reference { referenceName
                                                           , referencePosition
                                                           } =
  getFragment reference fragments >>=
  castFragmentType (Just referenceName) referencePosition allowedTargets

usedFragments :: FragmentLib -> [(Text, RawSelection)] -> [Node]
usedFragments fragments = concatMap findAllUses
  where
    findAllUses :: (Text, RawSelection) -> [Node]
    findAllUses (_, RawSelectionSet RawSelection' {rawSelectionRec}) =
      concatMap findAllUses rawSelectionRec
    findAllUses (_, RawAlias {rawAliasSelection}) =
      concatMap findAllUses [rawAliasSelection]
    findAllUses (_, InlineFragment Fragment {fragmentSelection}) =
      concatMap findAllUses fragmentSelection
    findAllUses (_, RawSelectionField {}) = []
    findAllUses (_, Spread Reference {referenceName, referencePosition}) =
      [EnhancedKey referenceName referencePosition] <> searchInFragment
      where
        searchInFragment =
          maybe
            []
            (concatMap findAllUses . fragmentSelection)
            (lookup referenceName fragments)

scanForSpread :: (Text, RawSelection) -> [Node]
scanForSpread (_, RawSelectionSet RawSelection' {rawSelectionRec = selection'}) =
  concatMap scanForSpread selection'
scanForSpread (_, RawAlias {rawAliasSelection = selection'}) =
  concatMap scanForSpread [selection']
scanForSpread (_, InlineFragment Fragment {fragmentSelection = selection'}) =
  concatMap scanForSpread selection'
scanForSpread (_, RawSelectionField {}) = []
scanForSpread (_, Spread Reference { referenceName = name'
                                   , referencePosition = position'
                                   }) = [EnhancedKey name' position']

validateFragment :: DataTypeLib -> (Text, Fragment) -> Validation NodeEdges
validateFragment lib (fName, Fragment { fragmentSelection
                                      , fragmentType
                                      , fragmentPosition
                                      }) =
  existsObjectType fragmentPosition fragmentType lib >>
  pure
    ( EnhancedKey fName fragmentPosition
    , concatMap scanForSpread fragmentSelection)

detectLoopOnFragments :: Graph -> Validation ()
detectLoopOnFragments lib = mapM_ checkFragment lib
  where
    checkFragment (fragmentID, _) = checkForCycle lib fragmentID [fragmentID]

checkForCycle :: Graph -> Node -> [Node] -> Validation Graph
checkForCycle lib parentNode history =
  case lookup parentNode lib of
    Just node -> concat <$> mapM checkNode node
    Nothing   -> pure []
  where
    checkNode x =
      if x `elem` history
        then cycleError x
        else recurse x
    recurse node = checkForCycle lib node $ history ++ [node]
    cycleError n = Left $ cannotSpreadWithinItself $ history ++ [n]
