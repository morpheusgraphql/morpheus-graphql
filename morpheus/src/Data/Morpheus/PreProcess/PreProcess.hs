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
import           Data.Morpheus.PreProcess.Utils     (existsType, fieldOf, typeBy)
import           Data.Morpheus.PreProcess.Variable  (checkQueryVariables)
import           Data.Morpheus.Types.Error          (MetaValidation)
import           Data.Morpheus.Types.Introspection  (GQLTypeLib, GQL__Type)
import           Data.Morpheus.Types.MetaInfo       (MetaInfo (..))
import           Data.Morpheus.Types.Types          (GQLOperator (..), GQLQueryRoot (..),
                                                     QuerySelection (..), SelectionSet, Validation)
import qualified Data.Set                           as S
import           Data.Text                          (Text, pack)

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ selectionError err
asGQLError (Right value) = pure value

mapSelectors :: GQLTypeLib -> GQLQueryRoot -> GQL__Type -> SelectionSet -> Validation SelectionSet
mapSelectors typeLib root _type selectors =
  spreadFieldsWhile root selectors >>= checkDuplicates >>= mapM (validateBySchema typeLib root _type)

validateBySchema ::
     GQLTypeLib -> GQLQueryRoot -> GQL__Type -> (Text, QuerySelection) -> Validation (Text, QuerySelection)
validateBySchema typeLib root _parentType (_name, SelectionSet args selectors pos) = do
  _field <- fieldOf pos _parentType _name
  _type <-  asGQLError $ typeBy pos typeLib _parentType _name
  head' <- validateArguments typeLib root _field args
  selectors' <- mapSelectors typeLib root _type selectors
  pure (_name, SelectionSet head' selectors' pos)
validateBySchema typeLib root _parentType (_name, Field args field pos) = do
  _field <- fieldOf pos _parentType _name
  _checksIfHasType <- asGQLError $ typeBy pos typeLib _parentType _name
  head' <- validateArguments typeLib root _field args
  pure (_name, Field head' field pos)
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
  _ <- validateFragments lib root
  let (operator, SelectionSet args body pos) = getOperationInfo $ queryBody root
  _type <- asGQLError $ existsType operator lib
  _ <- checkQueryVariables lib root args
  selectors <- mapSelectors lib root _type body
  pure $ updateQuery (queryBody root) (SelectionSet [] selectors pos)
