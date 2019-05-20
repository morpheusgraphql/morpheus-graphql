{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Selection
  ( validateSelectionSet
  ) where

import           Data.Morpheus.Error.Selection            (cannotQueryField, duplicateQuerySelections, hasNoSubfields)
import           Data.Morpheus.Schema.TypeKind            (TypeKind (..))
import           Data.Morpheus.Types.Core                 (EnhancedKey (..))
import           Data.Morpheus.Types.Error                (Validation)
import           Data.Morpheus.Types.Internal.AST         (ASTField (..), ASTOutputField, ASTOutputObject, ASTType (..),
                                                           ASTTypeLib (..))
import           Data.Morpheus.Types.Query.Fragment       (Fragment, FragmentLib)
import qualified Data.Morpheus.Types.Query.Fragment       as F (Fragment (..))
import           Data.Morpheus.Types.Query.RawSelection   (RawSelection (..), RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection      (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types                (Variables)
import           Data.Morpheus.Validation.Arguments       (resolveArguments, validateArguments)
import           Data.Morpheus.Validation.Spread          (castFragmentType, resolveSpread)
import           Data.Morpheus.Validation.Utils.Selection (lookupFieldAsSelectionSet, lookupPossibleTypeKeys,
                                                           lookupPossibleTypes, lookupSelectionField, notObject)
import           Data.Morpheus.Validation.Utils.Utils     (checkNameCollision)
import           Data.Text                                (Text)

selToKey :: (Text, Selection) -> EnhancedKey
selToKey (sName, Field _ _ pos)          = EnhancedKey sName pos
selToKey (sName, SelectionSet _ _ pos)   = EnhancedKey sName pos
selToKey (sName, UnionSelection _ _ pos) = EnhancedKey sName pos

checkDuplicatesOn :: ASTOutputObject -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn ASTType {typeName = name'} keys = checkNameCollision enhancedKeys (map fst keys) error' >> pure keys
  where
    error' = duplicateQuerySelections name'
    enhancedKeys = map selToKey keys

validateSelectionSet ::
     ASTTypeLib -> FragmentLib -> Variables -> ASTOutputObject -> RawSelectionSet -> Validation SelectionSet
validateSelectionSet lib' fragments' variables' type' selection' =
  concat <$> mapM (validateSelection lib' fragments' variables' type') selection' >>= checkDuplicatesOn type'

castFragment :: ASTTypeLib -> FragmentLib -> Variables -> ASTOutputObject -> Fragment -> Validation SelectionSet
castFragment lib' fragments' variables' onType' fragment' =
  validateSelectionSet lib' fragments' variables' onType' (F.content fragment')

splitFragment :: FragmentLib -> Text -> [Text] -> (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
splitFragment fragments' _ posTypes' (_, Spread key' position') = do
  fragment' <- resolveSpread fragments' posTypes' position' key'
  return ([fragment'], [])
splitFragment _ _ _ ("__typename", RawField [] field' position') =
  return ([], [("__typename", Field [] field' position')])
splitFragment _ type' _ (key', RawSelectionSet _ _ position') = Left $ cannotQueryField key' type' position'
splitFragment _ type' _ (key', RawField _ _ position') = Left $ cannotQueryField key' type' position'
splitFragment _ _ posTypes' (key', InlineFragment target' selectors' position') = do
  validatedFragment' <- castFragmentType Nothing position' posTypes' fragment'
  return ([validatedFragment'], [])
  where
    fragment' = F.Fragment {F.key = key', F.target = target', F.position = position', F.content = selectors'}

categorizeType :: [Fragment] -> ASTOutputObject -> (ASTOutputObject, [Fragment])
categorizeType fragments' type' = (type', filter matches fragments')
  where
    matches fragment' = F.target fragment' == typeName type'

categorizeTypes :: [ASTOutputObject] -> [Fragment] -> [(ASTOutputObject, [Fragment])]
categorizeTypes types' fragments' = map (categorizeType fragments') types'

flatTuple :: [([a], [b])] -> ([a], [b])
flatTuple list' = (concatMap fst list', concatMap snd list')

validateSelection ::
     ASTTypeLib -> FragmentLib -> Variables -> ASTOutputObject -> (Text, RawSelection) -> Validation SelectionSet
validateSelection lib' fragments' variables' parent' (key', RawSelectionSet rawArgs rawSelectors position') = do
  field' <- lookupSelectionField position' key' parent'
  resolvedArgs' <- resolveArguments variables' rawArgs
  arguments' <- validateArguments lib' (key', field') position' resolvedArgs'
  case fieldKind field' of
    UNION -> do
      keys' <- lookupPossibleTypeKeys position' key' lib' field'
      (spreads', __typename') <- flatTuple <$> mapM (splitFragment fragments' (typeName parent') keys') rawSelectors
      possibleFieldTypes' <- lookupPossibleTypes position' key' lib' keys'
      let zippedSpreads' = categorizeTypes possibleFieldTypes' spreads'
      unionSelections' <- mapM (validateCategory __typename') zippedSpreads'
      pure [(key', UnionSelection arguments' unionSelections' position')]
      where validateCategory :: SelectionSet -> (ASTOutputObject, [Fragment]) -> Validation (Text, SelectionSet)
            validateCategory sysSelection' (type', frags') = do
              selection' <- validateSelectionSet lib' fragments' variables' type' (concatMap F.content frags')
              return (typeName type', sysSelection' ++ selection')
    OBJECT -> do
      fieldType' <- lookupFieldAsSelectionSet position' key' lib' field'
      selections' <- validateSelectionSet lib' fragments' variables' fieldType' rawSelectors
      pure [(key', SelectionSet arguments' selections' position')]
    _ -> Left $ hasNoSubfields key' (fieldType field') position'
validateSelection lib' _ variables' parent' (key', RawField rawArgs field position') = do
  field' <- lookupSelectionField position' key' parent' >>= notObject (key', position')
  args' <- resolveArguments variables' rawArgs
  arguments' <- validateArguments lib' (key', field') position' args'
  pure [(key', Field arguments' field position')]
validateSelection lib' fragments' variables' parent' (key', Spread _ position') =
  resolveSpread fragments' [typeName parent'] position' key' >>= castFragment lib' fragments' variables' parent'
validateSelection lib' fragments' variables' parent' (key', InlineFragment target' selectors' position') =
  validateType >>= castFragment lib' fragments' variables' parent'
  where
    validateType = castFragmentType Nothing position' [typeName parent'] fragment'
    fragment' = F.Fragment {F.key = key', F.target = target', F.position = position', F.content = selectors'}
