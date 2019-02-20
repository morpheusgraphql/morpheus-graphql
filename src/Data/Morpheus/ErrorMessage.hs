{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.ErrorMessage
    ( syntaxError
    , unknownFragment
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
import           Data.Morpheus.Types.Types      ( MetaInfo(..) )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                )
import           Data.Data                      ( dataTypeOf
                                                , dataTypeName
                                                , Data
                                                )

errorMessage :: Text -> [GQLError]
errorMessage x = [GQLError { message = x, locations = [ErrorLocation 0 0] }]

handleError x = Left $ errorMessage $ concat ["Field Error: ", x]

variableIsNotDefined :: MetaInfo -> [GQLError]
variableIsNotDefined meta = errorMessage $ concat ["Variable ", key meta, " is not defined by operation ", className meta, "."]

unknownFragment :: MetaInfo -> [GQLError]
unknownFragment meta = errorMessage $ concat [ "Unknown fragment " , key meta, "."]

requiredArgument :: MetaInfo -> [GQLError]
requiredArgument meta = errorMessage $ concat
    ["Required Argument: ", key meta, "not Found on type ", className meta]

cannotQueryField :: MetaInfo -> [GQLError]
cannotQueryField meta = errorMessage $ concat
    ["Cannot query field ", key meta, " on type ", className meta, "."]

subfieldsNotSelected :: a -> Text -> [GQLError]
subfieldsNotSelected record key = errorMessage $ concat
    ["Field ", key, " of type \"Type\" must have a selection of subfields"]

syntaxError :: Text -> [GQLError]
syntaxError e = errorMessage $ concat ["Syntax Error: ", e]

semanticError :: Text -> [GQLError]
semanticError = errorMessage
