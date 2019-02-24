{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus.PreProcess
    ( preProcessQuery
    )
where

import           Data.List                      ( find )
import qualified Data.Map                      as M
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.Morpheus.Types.Types      ( Eval(..)
                                                , (::->)(..)
                                                , JSType(..)
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , Arguments(..)
                                                , FragmentLib
                                                , Fragment(..)
                                                , MetaInfo(..)
                                                , Argument(..)
                                                , GQLQueryRoot(..)
                                                )
import           Data.Morpheus.ErrorMessage     ( semanticError
                                                , handleError
                                                , cannotQueryField
                                                , requiredArgument
                                                ,unknownFragment
                                                , variableIsNotDefined
                                                )

import           Data.Proxy
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(fields, name)
                                                , GQL__Field
                                                , emptyLib
                                                , GQLTypeLib
                                                , GQL__InputValue
                                                )
import           Data.Morpheus.Schema.SchemaField
                                                ( getFieldTypeByKey
                                                , fieldArgsByKey
                                                )
import           Data.Morpheus.Schema.InputValue
                                                ( inputValueName , isRequired)

existsType :: Text -> GQLTypeLib -> Eval GQL__Type
existsType typeName typeLib = case M.lookup typeName typeLib of
    Nothing -> handleError $ pack $ "type does not exist" ++ unpack typeName
    Just x  -> pure x

-- TODO: replace all var types with Variable values
replaceVariable :: GQLQueryRoot -> Argument -> Eval Argument
replaceVariable root (Variable key) =
    case M.lookup key (inputVariables root) of
        Nothing    -> Left  $ variableIsNotDefined  $ MetaInfo {
          className="TODO: Name",
          cons = "",
          key = key
        }
        Just value -> pure $ Argument $ JSString value
replaceVariable _ x = pure x

validateArgument
    :: GQLQueryRoot -> Arguments -> GQL__InputValue -> Eval (Text, Argument)
validateArgument root requestArgs inpValue =
    case lookup (inputValueName inpValue) requestArgs of
        Nothing -> case isRequired inpValue of
          True -> Left $ requiredArgument $ MetaInfo
              { className = "TODO: name"
              , cons      = ""
              , key       = pack $ show $ inputValueName inpValue
              }
          False -> pure (inputValueName inpValue, Argument JSNull)
        Just x -> replaceVariable root x >>= \x -> pure (key, x)
            where key = inputValueName inpValue

-- TODO: throw Error when gql request has more arguments al then inputType
validateArguments
    :: GQLQueryRoot -> [GQL__InputValue] -> Arguments -> Eval Arguments
validateArguments root _types args = mapM (validateArgument root args) _types

fieldOf :: GQL__Type -> Text -> Eval GQL__Type
fieldOf _type fieldName = case getFieldTypeByKey fieldName _type of
    Nothing    -> Left $ cannotQueryField $ MetaInfo {
      key = fieldName
      , cons = ""
      , className = name _type
    }
    Just fieldType -> pure fieldType

validateSpread :: FragmentLib -> Text -> Eval [(Text, QuerySelection)]
validateSpread frags key = case M.lookup key frags of
    Nothing -> Left $ unknownFragment $ MetaInfo {
                className = ""
                , cons      = ""
                , key       = key
             }
    Just (Fragment _ _ (SelectionSet _ gqlObj)) -> pure gqlObj

propagateSpread
    :: GQLQueryRoot -> (Text, QuerySelection) -> Eval [(Text, QuerySelection)]
propagateSpread root (key , Spread _) = validateSpread (fragments root) key
propagateSpread root (text, value     ) = pure [(text, value)]

typeBy typeLib _parentType _name = fieldOf _parentType _name >>= fieldType
    where fieldType field = existsType (name field) typeLib

argsType :: GQL__Type -> Text -> Eval [GQL__InputValue]
argsType currentType key = case fieldArgsByKey key currentType of
    Nothing   -> handleError $ pack $ "header not found: " ++ show key
    Just args -> pure args

mapSelectors
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> SelectionSet
    -> Eval SelectionSet
mapSelectors typeLib root _type selectors = do
  selectors' <- concat <$> mapM (propagateSpread root) selectors

  mapM (validateBySchema typeLib root _type) selectors'

validateBySchema
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Eval (Text, QuerySelection)
validateBySchema typeLib root _parentType (_name, SelectionSet head selectors)
    = do
        _type      <- typeBy typeLib _parentType _name
        _argsType  <- argsType _parentType _name
        head'      <- validateArguments root _argsType head
        selectors' <- mapSelectors typeLib root _type selectors
        pure (_name, SelectionSet head' selectors')

validateBySchema typeLib root _parentType (_name, Field head field) = do
    _checksIfhasType  <- typeBy typeLib _parentType _name
    _argsType <- argsType _parentType _name
    head'     <- validateArguments root _argsType head
    pure (_name, Field head' field)

validateBySchema _ _ _ x = pure x

preProcessQuery :: GQLTypeLib -> GQLQueryRoot -> Eval QuerySelection
preProcessQuery lib root = do
    _type <- existsType "Query" lib
    let (SelectionSet _ body) = queryBody root
    selectors <- mapSelectors lib root _type body
    pure $ SelectionSet [] selectors
