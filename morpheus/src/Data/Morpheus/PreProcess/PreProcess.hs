{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.PreProcess.PreProcess
  ( preProcessQuery
  ) where

import           Data.List                          ((\\))
import           Data.Morpheus.Error.Selection      (cannotQueryField, selectionError)
import           Data.Morpheus.PreProcess.Arguments (validateArguments)
import           Data.Morpheus.PreProcess.Fragment  (validateFragments)
import           Data.Morpheus.PreProcess.Spread    (spreadFieldsWhile)
import           Data.Morpheus.PreProcess.Utils     (existsType, fieldOf, fieldType)
import           Data.Morpheus.PreProcess.Variable  (validateVariables)
import           Data.Morpheus.Types.Error          (Validation, MetaValidation)
import           Data.Morpheus.Types.Introspection  (GQLTypeLib, GQL__Type)
import           Data.Morpheus.Types.MetaInfo       (MetaInfo (..))
import           Data.Morpheus.Types.Types          (GQLOperator (..), GQLQueryRoot (..),
                                                     QuerySelection (..), SelectionSet)
import qualified Data.Set                           as S
import           Data.Text                          (Text, pack)
import           Data.Morpheus.Error.Utils          (toGQLError)

asSelectionValidation :: MetaValidation a -> Validation a
asSelectionValidation = toGQLError selectionError

mapSelectors :: GQLTypeLib -> GQLQueryRoot -> GQL__Type -> SelectionSet -> Validation SelectionSet
mapSelectors typeLib root _type selectors =
  spreadFieldsWhile root selectors >>= checkDuplicates >>= mapM (validateBySchema typeLib root _type)

validateBySchema ::
     GQLTypeLib -> GQLQueryRoot -> GQL__Type -> (Text, QuerySelection) -> Validation (Text, QuerySelection)
validateBySchema typeLib root _parentType (sName, SelectionSet args selectors pos) = do
  fieldSD <- asSelectionValidation $ fieldOf pos _parentType sName
  typeSD <- asSelectionValidation $ fieldType pos typeLib fieldSD
  headQS <- validateArguments typeLib root fieldSD args
  selectorsQS <- mapSelectors typeLib root typeSD selectors
  pure (sName, SelectionSet headQS selectorsQS pos)
validateBySchema typeLib root _parentType (sName, Field args field pos) = do
  fieldSD <- asSelectionValidation $ fieldOf pos _parentType sName
  _checksIfHasType <- asSelectionValidation $ fieldType pos typeLib fieldSD
  head' <- validateArguments typeLib root fieldSD args
  pure (sName, Field head' field pos)
validateBySchema _ _ _ x = pure x

checkDuplicates :: [(Text, a)] -> Validation [(Text, a)]
checkDuplicates x =
  case keys \\ noDuplicates keys of
    []         -> pure x
    duplicates -> Left $ cannotQueryField $ meta duplicates
  where
    keys = map fst x
    noDuplicates = S.toList . S.fromList
    meta duplicates = MetaInfo {typeName = "-- TODO: Error handling", key = pack $ show duplicates, position = 0}

getOperationInfo :: GQLOperator -> (Text, QuerySelection)
getOperationInfo (QueryOperator _ x)    = ("Query", x)
getOperationInfo (MutationOperator _ x) = ("Mutation", x)

updateQuery :: GQLOperator -> QuerySelection -> GQLOperator
updateQuery (QueryOperator name _)    = QueryOperator name
updateQuery (MutationOperator name _) = MutationOperator name

preProcessQuery :: GQLTypeLib -> GQLQueryRoot -> Validation GQLOperator
preProcessQuery lib root = do
  let (operator, SelectionSet args body pos) = getOperationInfo $ queryBody root
  validateVariables lib root args
  validateFragments lib root
  _type <- asSelectionValidation $ existsType operator lib
  selectors <- mapSelectors lib root _type body
  pure $ updateQuery (queryBody root) (SelectionSet [] selectors pos)
