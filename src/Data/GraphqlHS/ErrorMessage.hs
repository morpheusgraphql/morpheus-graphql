{-# LANGUAGE OverloadedStrings #-}

module Data.GraphqlHS.ErrorMessage
    ( syntaxError
    , unknownArgument
    , cannotQueryField
    , subfieldsNotSelected
    , handleError
    , semanticError
    , requiredArgument
    )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.List                      ( concat
                                                , intersperse
                                                )
import           Data.GraphqlHS.Types.Types     ( Eval(..)
                                                , MetaInfo(..)
                                                )
import           Data.GraphqlHS.Types.Error     ( GQLError(..)
                                                , ErrorLocation(..)
                                                )
import           Data.Data                      ( dataTypeOf
                                                , dataTypeName
                                                , Data
                                                )

throwNewError :: Text -> [GQLError]
throwNewError x = [GQLError { message = x, locations = [ErrorLocation 0 0] }]

handleError x = Fail $ throwNewError $ pack ("Field Error: " ++ show x)

unknownArgument :: Data a => a -> [Text] -> [GQLError]
unknownArgument record list = throwNewError $ pack
    (  "Unknown argument "
    ++ (show $ head list)
    ++ " on field "
    ++ (show $ dataTypeName $ dataTypeOf $ record)
    ++ "."
    )

requiredArgument :: MetaInfo -> [GQLError]
requiredArgument meta =
    throwNewError
        $  pack
        $  "not Found Required Argument: "
        ++ (show $ key meta)
        ++ "on type "
        ++ (show $ className meta)

cannotQueryField :: Text -> Text -> [GQLError]
cannotQueryField fieldName typeName = throwNewError $ pack
    (  "Cannot query field "
    ++ (show $ fieldName)
    ++ " on type "
    ++ (show $ typeName)
    ++ "."
    )

subfieldsNotSelected :: a -> Text -> [GQLError]
subfieldsNotSelected record key = throwNewError $ pack
    (  "Field "
    ++ (show key)
    ++ " of type \"Type\" must have a selection of subfields"
    )

syntaxError :: Text -> [GQLError]
syntaxError e = throwNewError $ pack ("Syntax Error: " ++ show e)

semanticError :: Text -> [GQLError]
semanticError = throwNewError

notUseHead x =
    Left
        $  pack
        $  "Error on "
        ++ (show x)
        ++ "this type has should not have Header Arguments, Pleaase create method for it"