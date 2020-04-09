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
import           Data.Functor                   (($>))
import           Data.Foldable                  (traverse_) 

-- MORPHEUS
import           Data.Morpheus.Error.Fragment   ( cannotBeSpreadOnType
                                                , cannotSpreadWithinItself
                                                , unusedFragment
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Fragment(..)
                                                , Fragments
                                                , SelectionContent(..)
                                                , Selection(..)
                                                , Ref(..)
                                                , Position
                                                , SelectionSet
                                                , RAW
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( selectOr
                                                , toList
                                                , toAssoc
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validator
                                                ( Validator
                                                , askSchema
                                                , askFragments
                                                , selectKnown
                                                , constraint
                                                , Constraint(..)
                                                )


validateFragments :: SelectionSet RAW -> Validator ()
validateFragments selectionSet 
  = fragmentsCycleChecking 
    *> checkUnusedFragments selectionSet
    *> fragmentsConditionTypeChecking

checkUnusedFragments :: SelectionSet RAW -> Validator ()
checkUnusedFragments selectionSet = do
    fragments <- askFragments
    case refs fragments \\ usedFragments fragments (toAssoc selectionSet) of
      []     -> return ()
      unused -> failure (unusedFragment unused)
  where
    refs fragments = map toRef (toList fragments)
    toRef Fragment { fragmentName , fragmentPosition } = Ref fragmentName fragmentPosition

castFragmentType
  :: Maybe Text -> Position -> [Text] -> Fragment -> Validator Fragment
castFragmentType key' position' typeMembers fragment@Fragment { fragmentType }
  = if fragmentType `elem` typeMembers
    then pure fragment
    else failure $ cannotBeSpreadOnType key' fragmentType position' typeMembers

resolveSpread :: [Text] -> Ref -> Validator Fragment
resolveSpread allowedTargets ref@Ref { refName, refPosition } 
  = askFragments
    >>= selectKnown ref
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


fragmentsConditionTypeChecking :: Validator ()
fragmentsConditionTypeChecking = do
    fragments <- askFragments
    traverse_ checkTypeExistence (toList fragments)

checkTypeExistence :: Fragment -> Validator ()
checkTypeExistence fr@Fragment { fragmentType, fragmentPosition }
      = askSchema
        >>= selectKnown (Ref fragmentType fragmentPosition) 
        >>= constraint OBJECT fr 
        >> pure ()

fragmentsCycleChecking :: Validator ()
fragmentsCycleChecking = exploreSpreads >>= fragmentCycleChecking 

exploreSpreads :: Validator Graph
exploreSpreads =  map exploreFragmentSpreads . toList <$> askFragments

exploreFragmentSpreads :: Fragment -> NodeEdges 
exploreFragmentSpreads Fragment { fragmentName, fragmentSelection, fragmentPosition }
   = (ref, concatMap scanForSpread fragmentSelection) 
  where     ref = Ref fragmentName fragmentPosition

scanForSpread :: Selection RAW -> [Node]
scanForSpread Selection { selectionContent = SelectionField } = []
scanForSpread Selection { selectionContent = SelectionSet selectionSet } =
  concatMap scanForSpread selectionSet
scanForSpread (InlineFragment Fragment { fragmentSelection }) =
  concatMap scanForSpread fragmentSelection
scanForSpread (Spread Ref { refName, refPosition }) =
  [Ref refName refPosition]

type Node = Ref

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

fragmentCycleChecking :: Graph -> Validator ()
fragmentCycleChecking lib = traverse_ checkFragment lib
 where
  checkFragment (fragmentID, _) = checkForCycle lib fragmentID [fragmentID]

checkForCycle :: Graph -> Node -> [Node] -> Validator Graph
checkForCycle lib parentNode history = case lookup parentNode lib of
  Just node -> concat <$> traverse checkNode node
  Nothing   -> pure []
 where
  checkNode x = if x `elem` history then cycleError x else recurse x
  recurse node = checkForCycle lib node $ history ++ [node]
  cycleError n = failure $ cannotSpreadWithinItself (n : history)
