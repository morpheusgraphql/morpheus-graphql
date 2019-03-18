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
import           Data.Morpheus.PreProcess.Spread        (spreadFields)
import           Data.Morpheus.PreProcess.Utils         (existsType, fieldOf, fieldType)
import           Data.Morpheus.PreProcess.Variable      (validateVariables)
import           Data.Morpheus.Schema.Utils.Utils       (Type, TypeLib)
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..))
import           Data.Morpheus.Types.Query.Operator     (Operator (..), RawOperator, ValidOperator)
import           Data.Morpheus.Types.Query.RawSelection (RawArguments, RawSelection (..),
                                                         RawSelectionSet)
import           Data.Morpheus.Types.Types              (Arguments, GQLQueryRoot (..),
                                                         QuerySelection (..), SelectionSet)
import qualified Data.Set                               as S
import           Data.Text                              (Text, pack)

asSelectionValidation :: MetaValidation a -> Validation a
asSelectionValidation = toGQLError selectionError

mapSelectors :: TypeLib -> GQLQueryRoot -> Type -> RawSelectionSet -> Validation SelectionSet
mapSelectors typeLib root _type selectors =
  spreadFields root selectors >>= checkDuplicates >>= mapM (validateBySchema typeLib root _type)

validateBySchema :: TypeLib -> GQLQueryRoot -> Type -> (Text, RawSelection) -> Validation (Text, QuerySelection)
validateBySchema typeLib root _parentType (sName, RawSelectionSet args selectors pos) = do
  fieldSD <- asSelectionValidation $ fieldOf pos _parentType sName
  typeSD <- asSelectionValidation $ fieldType pos typeLib fieldSD
  headQS <- validateArguments typeLib root fieldSD args
  selectorsQS <- mapSelectors typeLib root typeSD selectors
  pure (sName, SelectionSet headQS selectorsQS pos)
validateBySchema typeLib root _parentType (sName, RawField args field pos) = do
  fieldSD <- asSelectionValidation $ fieldOf pos _parentType sName
  _checksIfHasType <- asSelectionValidation $ fieldType pos typeLib fieldSD
  headQS <- validateArguments typeLib root fieldSD args
  pure (sName, Field headQS field pos)

--validateBySchema _ _ _ x = Left $ handleError "unresolved Spread"
checkDuplicates :: [(Text, a)] -> Validation [(Text, a)]
checkDuplicates x =
  case keys \\ noDuplicates keys of
    []         -> pure x
    duplicates -> Left $ cannotQueryField $ meta duplicates
  where
    keys = map fst x
    noDuplicates = S.toList . S.fromList
    meta duplicates = MetaInfo {typeName = "-- TODO: Error handling", key = pack $ show duplicates, position = 0}

getOperationInfo :: RawOperator -> (Text, RawArguments, RawSelectionSet)
getOperationInfo (Query _ args sel _)    = ("Query", args, sel)
getOperationInfo (Mutation _ args sel _) = ("Mutation", args, sel)

updateQuery :: RawOperator -> SelectionSet -> ValidOperator
updateQuery (Query name _ _ pos) sel    = Query name [] sel pos -- TODO: real args
updateQuery (Mutation name _ _ pos) sel = Mutation name [] sel pos -- TODO: real args

preProcessQuery :: TypeLib -> GQLQueryRoot -> Validation ValidOperator
preProcessQuery lib root = do
  let (operator, args, sel) = getOperationInfo $ queryBody root
  validateVariables lib root args
  validateFragments lib root
  _type <- asSelectionValidation $ existsType operator lib
  selectors <- mapSelectors lib root _type sel
  pure $ updateQuery (queryBody root) selectors
