{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.PreProcess.PreProcess
  ( preProcessQuery
  ) where

import           Data.Morpheus.Error.Selection          (duplicateQuerySelections, selectionError)
import           Data.Morpheus.Error.Utils              (toGQLError)
import           Data.Morpheus.PreProcess.Arguments     (validateArguments)
import           Data.Morpheus.PreProcess.Fragment      (validateFragments)
import           Data.Morpheus.PreProcess.Spread        (prepareRawSelection)
import           Data.Morpheus.PreProcess.Utils         (differKeys, existsObjectType, fieldOf)
import           Data.Morpheus.PreProcess.Variable      (validateVariables)
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), ObjectField (..), TypeLib)
import qualified Data.Morpheus.Schema.Internal.Types    as SC (Field (..))
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.MetaInfo           (Position)
import           Data.Morpheus.Types.Query.Operator     (Operator (..), RawOperator, ValidOperator)
import           Data.Morpheus.Types.Query.RawSelection (RawArguments, RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import qualified Data.Set                               as S
import           Data.Text                              (Text)

asSelectionValidation :: MetaValidation a -> Validation a
asSelectionValidation = toGQLError selectionError

mapSelectors :: TypeLib -> GObject ObjectField -> SelectionSet -> Validation SelectionSet
mapSelectors typeLib type' selectors = checkDuplicatesOn type' selectors >>= mapM (validateBySchema typeLib type')

validateBySchema :: TypeLib -> GObject ObjectField -> (Text, Selection) -> Validation (Text, Selection)
validateBySchema lib' (GObject parentFields core) (name', SelectionSet args' selectors sPos) = do
  field' <- asSelectionValidation $ fieldOf (sPos, name core) parentFields name'
  typeSD <- asSelectionValidation $ existsObjectType (sPos, name') (SC.fieldType $ fieldContent field') lib'
  headQS <- validateArguments lib' (name', field') sPos args'
  selectorsQS <- mapSelectors lib' typeSD selectors
  pure (name', SelectionSet headQS selectorsQS sPos)
validateBySchema typeLib (GObject parentFields core) (name', Field args' field sPos) = do
  field' <- asSelectionValidation $ fieldOf (sPos, name core) parentFields name'
  -- _checksIfHasType <- asSelectionValidation $ getObjectFieldType sPos typeLib fieldSD
  headQS <- validateArguments typeLib (name', field') sPos args'
  pure (name', Field headQS field sPos)

selToKey :: (Text, Selection) -> EnhancedKey
selToKey (sName, Field _ _ pos)        = EnhancedKey sName pos
selToKey (sName, SelectionSet _ _ pos) = EnhancedKey sName pos

checkDuplicatesOn :: GObject ObjectField -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn (GObject _ core) keys =
  case differKeys enhancedKeys noDuplicates of
    []         -> pure keys
    duplicates -> Left $ duplicateQuerySelections (name core) duplicates
  where
    enhancedKeys = map selToKey keys
    noDuplicates = S.toList $ S.fromList (map fst keys)

getOperationInfo :: RawOperator -> (Text, RawArguments, RawSelectionSet, Position)
getOperationInfo (Query _ args' sel pos)    = ("Query", args', sel, pos)
getOperationInfo (Mutation _ args' sel pos) = ("Mutation", args', sel, pos)

updateQuery :: RawOperator -> SelectionSet -> ValidOperator
updateQuery (Query name' _ _ pos) sel    = Query name' [] sel pos
updateQuery (Mutation name' _ _ pos) sel = Mutation name' [] sel pos

preProcessQuery :: TypeLib -> GQLQueryRoot -> Validation ValidOperator
preProcessQuery lib root = do
  let (operator, args', rawSel, position') = getOperationInfo $ queryBody root
  validateVariables lib root args'
  validateFragments lib root
  _type <- asSelectionValidation $ existsObjectType (position', operator) operator lib
  sel <- prepareRawSelection root rawSel
  selectors <- mapSelectors lib _type sel
  pure $ updateQuery (queryBody root) selectors
