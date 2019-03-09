{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus.PreProcess.PreProcess
    ( preProcessQuery
    )
where

import qualified Data.Set                      as S
import           Data.List                      ( find
                                                , (\\)
                                                )
import qualified Data.Map                      as M
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , (::->)(..)
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , Arguments(..)
                                                , FragmentLib
                                                , Fragment(..)
                                                , Argument(..)
                                                , GQLQueryRoot(..)
                                                , EnumOf(..)
                                                , GQLOperator(..)
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..)
                                                , Position(..)
                                                )
import           Data.Morpheus.ErrorMessage     ( handleError
                                                , cannotQueryField
                                                , requiredArgument
                                                , variableIsNotDefined
                                                , invalidEnumOption
                                                )
import           Data.Morpheus.Schema.GQL__TypeKind
                                                ( GQL__TypeKind(..) )
import           Data.Morpheus.Schema.GQL__EnumValue
                                                ( isEnumOf )
import           Data.Proxy
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(..)
                                                , GQL__Field
                                                , emptyLib
                                                , GQLTypeLib
                                                , GQL__InputValue
                                                )
import           Data.Morpheus.Schema.SchemaField
                                                ( getFieldTypeByKey
                                                , fieldArgsByKey
                                                , selectFieldByKey
                                                )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
import qualified Data.Morpheus.Schema.InputValue
                                               as I
                                                ( name
                                                , inputValueMeta
                                                , isRequired
                                                , typeName
                                                )
import           Data.Morpheus.PreProcess.Spread
                                                ( spreadFieldsWhile )
import           Data.Morpheus.PreProcess.Utils ( existsType
                                                , typeBy
                                                , fieldOf
                                                )
import           Data.Morpheus.PreProcess.Arguments
                                                ( validateArguments )
import           Data.Morpheus.PreProcess.Variable
                                                ( checkQueryVariables )

import           Data.Morpheus.PreProcess.Fragment
                                                ( validateFragments )



mapSelectors
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> SelectionSet
    -> Validation SelectionSet
mapSelectors typeLib root _type selectors =
    spreadFieldsWhile root selectors >>= checkDuplicates >>= mapM
        (validateBySchema typeLib root _type)

validateBySchema
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Validation (Text, QuerySelection)
validateBySchema typeLib root _parentType (_name, SelectionSet head selectors)
    = do
        _field     <- fieldOf (lineMarks root) (Position 0) _parentType _name
        _type      <- typeBy (lineMarks root) typeLib _parentType _name
        head'      <- validateArguments typeLib root _field head
        selectors' <- mapSelectors typeLib root _type selectors
        pure (_name, SelectionSet head' selectors')

validateBySchema typeLib root _parentType (_name, Field head field pos) = do
    _field           <- fieldOf (lineMarks root) pos _parentType _name
    _checksIfHasType <- typeBy (lineMarks root) typeLib _parentType _name
    head'            <- validateArguments typeLib root _field head
    pure (_name, Field head' field pos)

validateBySchema _ _ _ x = pure x

checkDuplicates :: [(Text, a)] -> Validation [(Text, a)]
checkDuplicates x = case keys \\ noDuplicates keys of
    []         -> pure x
    duplicates -> Left $ cannotQueryField [] $ meta duplicates
  where
    keys         = map fst x
    noDuplicates = S.toList . S.fromList
    meta duplicates = MetaInfo
        { typeName = "-- TODO: Error handling"
        , key      = pack $ show duplicates
        , position = Position 0
        }


getOperationInfo (QueryOperator    name x) = ("Query", x)
getOperationInfo (MutationOperator name x) = ("Mutation", x)

updateQuery :: GQLOperator -> QuerySelection -> GQLOperator
updateQuery (QueryOperator    name _) = QueryOperator name
updateQuery (MutationOperator name _) = MutationOperator name

preProcessQuery :: GQLTypeLib -> GQLQueryRoot -> Validation GQLOperator
preProcessQuery lib root = do
    validateFragments lib root
    let (operator, SelectionSet args body) = getOperationInfo $ queryBody root
    _type     <- existsType operator lib
    variable  <- checkQueryVariables lib root args
    selectors <- mapSelectors lib root _type body
    pure $ updateQuery (queryBody root) (SelectionSet [] selectors)
