module Data.Morpheus.Validation.Fragment
  ( validateFragments
  ) where

import qualified Data.Map                               as M (toList)
import           Data.Morpheus.Error.Fragment           (cannotSpreadWithinItself)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.Internal.AST       (ASTTypeLib)
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..))
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..))
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Utils.Utils   (existsObjectType)
import           Data.Text                              (Text)

type Node = EnhancedKey

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

scanForSpread :: ASTTypeLib -> GQLQueryRoot -> (Text, RawSelection) -> [Node]
scanForSpread lib' root' (_, RawSelectionSet _ selectors _) = concatMap (scanForSpread lib' root') selectors
scanForSpread lib' root' (_, InlineFragment _ selectors _)  = concatMap (scanForSpread lib' root') selectors
scanForSpread _ _ (_, RawField {})                          = []
scanForSpread _ _ (_, Spread value pos)                     = [EnhancedKey value pos]

validateFragment :: ASTTypeLib -> GQLQueryRoot -> (Text, Fragment) -> Validation NodeEdges
validateFragment lib' root (fName, Fragment {content = selection, target = target', position = position'}) =
  existsObjectType position' target' lib' >>
  pure (EnhancedKey fName position', concatMap (scanForSpread lib' root) selection)

validateFragments :: ASTTypeLib -> GQLQueryRoot -> Validation ()
validateFragments lib root = mapM (validateFragment lib root) (M.toList $ fragments root) >>= detectLoopOnFragments

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
