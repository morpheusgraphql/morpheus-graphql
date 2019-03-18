{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.PreProcess.PreProcess
  ( preProcessQuery
  ) where

import           Data.List                          ((\\))
import           Data.Morpheus.Error.Selection      (cannotQueryField, selectionError)
import           Data.Morpheus.Error.Utils          (toGQLError)
import           Data.Morpheus.PreProcess.Arguments (validateArguments)
import           Data.Morpheus.PreProcess.Fragment  (validateFragments)
import           Data.Morpheus.PreProcess.Spread    (spreadFieldsWhile)
import           Data.Morpheus.PreProcess.Utils     (existsType, fieldOf, fieldType)
import           Data.Morpheus.PreProcess.Variable  (validateVariables)
import           Data.Morpheus.Schema.Utils.Utils   (Type, TypeLib)
import           Data.Morpheus.Types.Error          (MetaValidation, Validation)
import           Data.Morpheus.Types.MetaInfo       (MetaInfo (..))
import           Data.Morpheus.Types.Query.Operator (Operator (..))
import           Data.Morpheus.Types.Types          (Arguments, GQLQueryRoot (..),
                                                     QuerySelection (..), SelectionSet)
import qualified Data.Set                           as S
import           Data.Text                          (Text, pack)

asSelectionValidation :: MetaValidation a -> Validation a
asSelectionValidation = toGQLError selectionError

mapSelectors :: TypeLib -> GQLQueryRoot -> Type -> SelectionSet -> Validation SelectionSet
mapSelectors typeLib root _type selectors =
  spreadFieldsWhile root selectors >>= checkDuplicates >>= mapM (validateBySchema typeLib root _type)

validateBySchema :: TypeLib -> GQLQueryRoot -> Type -> (Text, QuerySelection) -> Validation (Text, QuerySelection)
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

getOperationInfo :: Operator -> (Text, Arguments, SelectionSet)
getOperationInfo (Query _ args sel _)    = ("Query", args, sel)
getOperationInfo (Mutation _ args sel _) = ("Mutation", args, sel)

updateQuery :: Operator -> SelectionSet -> Operator
updateQuery (Query name args _ pos) sel    = Query name args sel pos
updateQuery (Mutation name args _ pos) sel = Mutation name args sel pos

preProcessQuery :: TypeLib -> GQLQueryRoot -> Validation Operator
preProcessQuery lib root = do
  let (operator, args, sel) = getOperationInfo $ queryBody root
  validateVariables lib root args
  validateFragments lib root
  _type <- asSelectionValidation $ existsType operator lib
  selectors <- mapSelectors lib root _type sel
  pure $ updateQuery (queryBody root) selectors
