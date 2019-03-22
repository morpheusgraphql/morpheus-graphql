{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.PreProcess.PreProcess
  ( preProcessQuery
  ) where

import           Data.List                              ((\\))
import           Data.Morpheus.Error.Selection          (cannotQueryField, selectionError)
import           Data.Morpheus.Error.Utils              (toGQLError)
import           Data.Morpheus.PreProcess.Arguments     (validateArguments)
import           Data.Morpheus.PreProcess.Fragment      (validateFragments)
import           Data.Morpheus.PreProcess.Spread        (prepareRawSelection)
import           Data.Morpheus.PreProcess.Utils         (existsType, fieldOf, fieldType)
import           Data.Morpheus.PreProcess.Variable      (validateVariables)
import           Data.Morpheus.Schema.Utils.Utils       (Type, TypeLib)
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..), Position)
import           Data.Morpheus.Types.Query.Operator     (Operator (..), RawOperator, ValidOperator)
import           Data.Morpheus.Types.Query.RawSelection (RawArguments, RawSelectionSet)
import           Data.Morpheus.Types.Query.Selection    (Selection (..), SelectionSet)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import qualified Data.Set                               as S
import           Data.Text                              (Text, pack)

asSelectionValidation :: MetaValidation a -> Validation a
asSelectionValidation = toGQLError selectionError

mapSelectors :: TypeLib -> Type -> SelectionSet -> Validation SelectionSet
mapSelectors typeLib _type selectors = checkDuplicates selectors >>= mapM (validateBySchema typeLib _type)

validateBySchema :: TypeLib -> Type -> (Text, Selection) -> Validation (Text, Selection)
validateBySchema typeLib _parentType (sName, SelectionSet args selectors pos) = do
  fieldSD <- asSelectionValidation $ fieldOf pos _parentType sName
  typeSD <- asSelectionValidation $ fieldType pos typeLib fieldSD
  headQS <- validateArguments typeLib fieldSD args
  selectorsQS <- mapSelectors typeLib typeSD selectors
  pure (sName, SelectionSet headQS selectorsQS pos)
validateBySchema typeLib _parentType (sName, Field args field pos) = do
  fieldSD <- asSelectionValidation $ fieldOf pos _parentType sName
  _checksIfHasType <- asSelectionValidation $ fieldType pos typeLib fieldSD
  headQS <- validateArguments typeLib fieldSD args
  pure (sName, Field headQS field pos)

checkDuplicates :: [(Text, a)] -> Validation [(Text, a)]
checkDuplicates x =
  case keys \\ noDuplicates keys of
    []         -> pure x
    duplicates -> Left $ cannotQueryField $ meta duplicates
  where
    keys = map fst x
    noDuplicates = S.toList . S.fromList
    meta duplicates = MetaInfo {typeName = "-- TODO: Error handling", key = pack $ show duplicates, position = 0}

getOperationInfo :: RawOperator -> (Text, RawArguments, RawSelectionSet, Position)
getOperationInfo (Query _ args sel pos)    = ("Query", args, sel, pos)
getOperationInfo (Mutation _ args sel pos) = ("Mutation", args, sel, pos)

updateQuery :: RawOperator -> SelectionSet -> ValidOperator
updateQuery (Query name _ _ pos) sel    = Query name [] sel pos
updateQuery (Mutation name _ _ pos) sel = Mutation name [] sel pos

preProcessQuery :: TypeLib -> GQLQueryRoot -> Validation ValidOperator
preProcessQuery lib root = do
  let (operator, args, rawSel, position') = getOperationInfo $ queryBody root
  validateVariables lib root args
  validateFragments lib root
  _type <- asSelectionValidation $ existsType (position',operator) operator lib
  sel <- prepareRawSelection root rawSel
  selectors <- mapSelectors lib _type sel
  pure $ updateQuery (queryBody root) selectors
