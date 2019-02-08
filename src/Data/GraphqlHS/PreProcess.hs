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
import           Data.GraphqlHS.Generics.Resolver
                                                ( Resolver(..) )
import           Data.GraphqlHS.Types.Types     ( Eval(..)
                                                , (::->)(..)
                                                , GQLType
                                                , GQLValue(..)
                                                , Head(..)
                                                , FragmentLib
                                                , Fragment(..)
                                                , MetaInfo(..)
                                                , Arg(..)
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
    Just x  -> Val x


validateSpread :: FragmentLib -> Text -> Eval [(Text, GQLValue)]
validateSpread frags key = case lookup key frags of
    Nothing -> handleError $ pack $ "Fragment not found: " ++ (show key)
    Just (Fragment _ _ (Object gqlObj)) -> Val (toList gqlObj)



validateFieldBody
    :: GQLTypeLib
    -> FragmentLib
    -> GQL__Type
    -> (Text, GQLValue)
    -> Eval (Text, GQLValue)
validateFieldBody typeLib frags currentType (fieldName, field) =
    case (fields currentType) of
        Some gqlVal -> case (getFieldTypeByKey fieldName currentType) of
            Nothing ->
                Fail $ cannotQueryField fieldName (name currentType)
            Just fieldType -> do
                value <- validateBySchema typeLib
                                          (name fieldType)
                                          frags
                                          field
                pure (fieldName, value)
        _ ->
            handleError $ pack $ "has not fields" ++ (show $ fields currentType)

handleField
    :: GQLTypeLib
    -> FragmentLib
    -> GQL__Type
    -> (Text, GQLValue)
    -> Eval (Text, GQLValue)
handleField typeLib frags currentType (fieldName, field) = case field of
    Query head0 body0 -> do
        head <- validateHead currentType fieldName head0
        body <- validateFieldBody typeLib frags currentType (fieldName, body0)
        pure $ (fieldName, Query head (snd body))
    _ -> validateFieldBody typeLib frags currentType (fieldName, field)

propagateSpread :: FragmentLib -> (Text, GQLValue) -> Eval [(Text, GQLValue)]
propagateSpread frags (key , (Spread _)) = validateSpread frags key
propagateSpread frags (text, value     ) = pure [(text, value)]

validateArg :: Map Text Arg -> GQL__InputValue -> Eval (Text, Text)
validateArg requestArgs inpValue =
    case (lookup (inputValueName inpValue) requestArgs) of
        Nothing -> Fail $ requiredArgument $ MetaInfo
            { className = ""
            , cons      = ""
            , key       = (pack $ show $ inputValueName inpValue)
            }
        Just x -> Val ("", "")

validateHead :: GQL__Type -> Text -> Head -> Eval Head
validateHead currentType key (Head args) =
    case (fieldArgsByKey key currentType) of
        Nothing    -> Fail $ cannotQueryField key (name currentType)
        Just field -> mapM (validateArg args) field >> Val (Head args)

validateBySchema :: GQLTypeLib -> Text -> FragmentLib -> GQLValue -> Eval GQLValue
validateBySchema typeLib typeName frags (Object gqlObj) = do
    extended <- concat <$> (mapM (propagateSpread frags) (toList gqlObj))
    existsType typeName typeLib
        >>= \x ->
                (Object . fromList)
                    <$> (mapM (handleField typeLib frags x) extended)

validateBySchema typeLib typeName frags (Query head body) =
    handleError $ pack $ "all query heads shcould be handled by Fields"

validateBySchema _ _ _ (Field value) = Val (Field value)

validateBySchema _ _ _ QNull         = Val QNull
