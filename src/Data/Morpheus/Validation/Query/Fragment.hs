{-# LANGUAGE GADTs              #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE FlexibleInstances  #-}

module Data.Morpheus.Validation.Query.Fragment
  ( validateFragments
  , castFragmentType
  , resolveSpread
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Foldable                  (traverse_) 

-- MORPHEUS
import           Data.Morpheus.Error.Fragment   ( cannotBeSpreadOnType
                                                , cannotSpreadWithinItself
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , Fragment(..)
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
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( BaseValidator
                                                , askSchema
                                                , askFragments
                                                , selectKnown
                                                , constraint
                                                , Constraint(..)
                                                , Validator
                                                , checkUnused
                                                )

validateFragments :: SelectionSet RAW -> BaseValidator ()
validateFragments selectionSet 
  = fragmentsCycleChecking 
    *> checkUnusedFragments selectionSet
    *> fragmentsConditionTypeChecking

checkUnusedFragments :: SelectionSet RAW -> BaseValidator ()
checkUnusedFragments selectionSet = do
    fragments <- askFragments
    checkUnused 
      (usedFragments fragments (toList selectionSet)) 
      (toList fragments)

castFragmentType
  :: Maybe Name -> Position -> [Name] -> Fragment -> Validator ctx Fragment
castFragmentType key position typeMembers fragment@Fragment { fragmentType }
  | fragmentType `elem` typeMembers = pure fragment
  | otherwise =  failure $ cannotBeSpreadOnType key fragmentType position typeMembers

resolveSpread :: [Name] -> Ref -> Validator ctx Fragment
resolveSpread allowedTargets ref@Ref { refName, refPosition } 
  = askFragments
    >>= selectKnown ref
    >>= castFragmentType (Just refName) refPosition allowedTargets

usedFragments :: Fragments -> [Selection RAW] -> [Node]
usedFragments fragments = concatMap findAllUses
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

fragmentsConditionTypeChecking :: BaseValidator ()
fragmentsConditionTypeChecking 
    = toList <$> askFragments
      >>= traverse_ checkTypeExistence

checkTypeExistence :: Fragment -> BaseValidator ()
checkTypeExistence fr@Fragment { fragmentType, fragmentPosition }
      = askSchema
        >>= selectKnown (Ref fragmentType fragmentPosition) 
        >>= constraint OBJECT fr 
        >> pure ()

fragmentsCycleChecking :: BaseValidator ()
fragmentsCycleChecking = exploreSpreads >>= fragmentCycleChecking 

exploreSpreads :: BaseValidator Graph
exploreSpreads =  map exploreFragmentSpreads . toList <$> askFragments

exploreFragmentSpreads :: Fragment -> NodeEdges 
exploreFragmentSpreads Fragment { fragmentName, fragmentSelection, fragmentPosition }
   = ( Ref fragmentName fragmentPosition, concatMap scanForSpread fragmentSelection)

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

fragmentCycleChecking :: Graph -> BaseValidator ()
fragmentCycleChecking lib = traverse_ checkFragment lib
 where
  checkFragment (fragmentID, _) = checkForCycle lib fragmentID [fragmentID]

checkForCycle :: Graph -> Node -> [Node] -> BaseValidator Graph
checkForCycle lib parentNode history = case lookup parentNode lib of
  Just node -> concat <$> traverse checkNode node
  Nothing   -> pure []
 where
  checkNode x = if x `elem` history then cycleError x else recurse x
  recurse node = checkForCycle lib node $ history ++ [node]
  cycleError n = failure $ cannotSpreadWithinItself (n : history)
