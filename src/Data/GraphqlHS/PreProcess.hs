{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.GraphqlHS.PreProcess
    ( validateBySchema
    )
where

import           Prelude                 hiding ( lookup )
import           Data.List                      ( find )
import           Data.Map                       ( elems
                                                , mapWithKey
                                                , lookup
                                                , toList
                                                , Map
                                                , fromList
                                                , keys
                                                )
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.GraphqlHS.Types.Types     ( Eval(..)
                                                , (::->)(..)
                                                , GQLType
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , Head(..)
                                                , FragmentLib
                                                , Fragment(..)
                                                , MetaInfo(..)
                                                , Arg(..)
                                                , GQLPrimitive(..)
                                                , GQLQueryRoot(..)
                                                )
import           Data.GraphqlHS.ErrorMessage    ( semanticError
                                                , handleError
                                                , cannotQueryField
                                                , requiredArgument
                                                )

import           Data.Proxy
import           Data.GraphqlHS.Types.Introspection
                                                ( GQL__Type(fields, name)
                                                , GQL__Field
                                                , emptyLib
                                                , GQLTypeLib
                                                , GQL__InputValue
                                                )
import           Data.GraphqlHS.Schema.SchemaField
                                                ( getFieldTypeByKey
                                                , selectFieldBykey
                                                , fieldArgsByKey
                                                )
import           Data.GraphqlHS.Schema.InputValue
                                                ( inputValueName )

existsType :: Text -> GQLTypeLib -> Eval GQL__Type
existsType typeName typeLib = case (lookup typeName typeLib) of
    Nothing -> handleError $ pack $ "type does not exist" ++ (unpack typeName)
    Just x  -> pure x


validateSpread :: FragmentLib -> Text -> Eval [(Text, QuerySelection)]
validateSpread frags key = case lookup key frags of
    Nothing -> handleError $ pack $ "Fragment not found: " ++ (show key)
    Just (Fragment _ _ (SelectionSet arguments gqlObj)) -> pure (toList gqlObj)


-- TODO: replace all var types with Variable values
replaceVariable :: GQLQueryRoot -> Arg -> Eval Arg
replaceVariable root (Var key) = case (lookup key (inputVariables root)) of
    Nothing    -> handleError $ pack $ "Variable not found: " ++ (show key)
    Just value -> pure $ ArgValue $ JSString value
replaceVariable _ x = pure $ x

validateArg
    :: GQLQueryRoot -> Map Text Arg -> GQL__InputValue -> Eval (Text, Arg)
validateArg root requestArgs inpValue =
    case (lookup (inputValueName inpValue) requestArgs) of
        Nothing -> Left $ requiredArgument $ MetaInfo
            { className = ""
            , cons      = ""
            , key       = (pack $ show $ inputValueName inpValue)
            }
        Just x -> replaceVariable root x >>= \x -> pure (key, x)
            where key = inputValueName inpValue

-- TODO: throw Error when gql request has more arguments al then inputType
validateHead :: GQLQueryRoot -> GQL__Type -> Text -> Head -> Eval Head
validateHead root currentType key (Arguments args) =
    case (fieldArgsByKey key currentType) of
        Nothing -> Left $ cannotQueryField key (name currentType)
        Just field ->
            mapM (validateArg root args) field >>= pure . Arguments . fromList


fieldOf :: GQL__Type -> Text -> Eval GQL__Type
fieldOf _type fieldName = case (fields _type) of
    Some gqlVal -> case (getFieldTypeByKey fieldName _type) of
        Nothing    -> Left $ cannotQueryField fieldName (name _type)
        Just ftype -> pure ftype
    _ -> handleError $ pack $ "has not fields" ++ (show $ fields _type)

propagateSpread
    :: GQLQueryRoot -> (Text, QuerySelection) -> Eval [(Text, QuerySelection)]
propagateSpread root (key , (Spread _)) = validateSpread (fragments root) key
propagateSpread root (text, value     ) = pure [(text, value)]


typeBy typeLib _parentType _name = fieldOf _parentType _name >>= fiedType
    where fiedType field = existsType (name field) typeLib

validateBySchema
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Eval (Text, QuerySelection)
validateBySchema typeLib root _parentType (_name, (SelectionSet head selectors))
    = do
        _type      <- typeBy typeLib _parentType _name
        head'      <- validateHead root _type _name head
        selectors' <- mapSelectors _type
        pure (_name, SelectionSet head' (fromList selectors'))
  where
    mapSelectors _type =
        concat <$> mapM (propagateSpread root) (toList selectors) >>= mapM
            (validateBySchema typeLib root _type)

validateBySchema typeLib root _parentType (_name, (Field head field)) = do
    _type <- typeBy typeLib _parentType _name
    head' <- validateHead root _type _name head
    pure (_name, Field head' field)

validateBySchema _ _ _ x = pure x


preProccessQuery :: GQLTypeLib -> GQLQueryRoot -> Eval QuerySelection
preProccessQuery lib root =
    validateBySchema lib root (queryBody root) ("Query", queryBody gqlRoot)
        >>= pure
        .   snd
