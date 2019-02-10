{-# LANGUAGE OverloadedStrings #-}

module Data.GraphqlHS.ErrorMessage
    ( syntaxError
    , unknownArgument
    , cannotQueryField
    , subfieldsNotSelected
    , handleError
    , semanticError
    , requiredArgument
    , errorMessage
    )
where

import           Prelude                 hiding ( concat )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                , concat
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

errorMessage = throwNewError

handleError x = Fail $ throwNewError $ concat ["Field Error: ", x]

unknownArgument :: Data a => a -> [Text] -> [GQLError]
unknownArgument record list = throwNewError $ concat
    [ "Unknown argument "
    , head list
    , " on field "
    , pack $ dataTypeName $ dataTypeOf $ record
    , "."
    ]

requiredArgument :: MetaInfo -> [GQLError]
requiredArgument meta = throwNewError $ concat
    ["Required Argument: ", key meta, "not Found on type ", className meta]

cannotQueryField :: Text -> Text -> [GQLError]
cannotQueryField key typeName = throwNewError
    $ concat ["Cannot query field ", key, " on type ", typeName, "."]

subfieldsNotSelected :: a -> Text -> [GQLError]
subfieldsNotSelected record key = throwNewError $ concat
    ["Field ", key, " of type \"Type\" must have a selection of subfields"]

syntaxError :: Text -> [GQLError]
syntaxError e = throwNewError $ concat ["Syntax Error: ", e]

semanticError :: Text -> [GQLError]
semanticError = throwNewError
