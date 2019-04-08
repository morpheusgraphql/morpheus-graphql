module Data.Morpheus.PreProcess.Fragment
  ( validateFragments
  ) where

import qualified Data.Map                               as M (toList)
import           Data.Morpheus.Error.Fragment           (cycleOnFragment, fragmentError)
import           Data.Morpheus.PreProcess.Utils         (existsObjectType)
import           Data.Morpheus.Schema.Internal.Types    (TypeLib)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.Query.Fragment     (Fragment (..))
import           Data.Morpheus.Types.Query.RawSelection (RawSelection (..))
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

type Node = EnhancedKey

type NodeEdges = (Node, [Node])

type Graph = [NodeEdges]

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ fragmentError err
asGQLError (Right value) = pure value

validateFragmentFields :: TypeLib -> GQLQueryRoot -> (Text, RawSelection) -> Validation [Node]
validateFragmentFields lib' root (_', RawSelectionSet _ selectors _) =
  concat <$> mapM (validateFragmentFields lib' root) selectors
validateFragmentFields _ _ (_, RawField {}) = pure []
validateFragmentFields _ _ (_, Spread value pos) = pure [EnhancedKey value pos]

validateFragment :: TypeLib -> GQLQueryRoot -> (Text, Fragment) -> Validation NodeEdges
validateFragment lib' root (fName, Fragment {content = selection, target = target', position = position'}) = do
  _type <- asGQLError $ existsObjectType (position', fName) target' lib'
  fragmentLinks <- concat <$> mapM (validateFragmentFields lib' root) selection
  pure (EnhancedKey fName position', fragmentLinks)

validateFragments :: TypeLib -> GQLQueryRoot -> Validation ()
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
    cycleError n = Left $ cycleOnFragment $ history ++ [n]
