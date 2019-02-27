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
    , variableIsNotDefined
    , unsupportedArgumentType
    , invalidEnumOption
    )
where

import qualified Data.Text as T (concat)
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                )
import           Data.Data                      ( dataTypeOf
                                                , dataTypeName
                                                , Data
                                                )

errorMessage :: Text -> [GQLError]
errorMessage x = [GQLError { message = x, locations = [ErrorLocation 0 0] }]

handleError x = Left $ errorMessage $ T.concat ["Field Error: ", x]

invalidEnumOption :: MetaInfo -> [GQLError]
invalidEnumOption meta = errorMessage $ T.concat ["Invalid Option \"", key meta, "\" on Enum \"", className meta, "\"."]

unsupportedArgumentType :: MetaInfo -> [GQLError]
unsupportedArgumentType meta = errorMessage $ T.concat ["Argument \"", key meta, "\" has unsuported type \"", className meta, "\"."]

variableIsNotDefined :: MetaInfo -> [GQLError]
variableIsNotDefined meta = errorMessage $ T.concat ["Variable \"", key meta, "\" is not defined by operation \"", className meta, "\"."]

unknownFragment :: MetaInfo -> [GQLError]
unknownFragment meta = errorMessage $ T.concat [ "Unknown fragment \"" , key meta, "\"."]

requiredArgument :: MetaInfo -> [GQLError]
requiredArgument meta = errorMessage $ T.concat
    ["Required Argument: \"", key meta, "\" not Found on type \"", className meta, "\"."]

cannotQueryField :: MetaInfo -> [GQLError]
cannotQueryField meta = errorMessage $ T.concat
    ["Cannot query field \"", key meta, "\" on type \"", className meta, "\"."]

subfieldsNotSelected :: MetaInfo -> [GQLError]
subfieldsNotSelected  meta = errorMessage $ T.concat
    ["Field \"", key meta, "\" of type \"", className meta ,"\" must have a selection of subfields"]

syntaxError :: Text -> [GQLError]
syntaxError e = errorMessage $ T.concat ["Syntax Error: ", e]

semanticError :: Text -> [GQLError]
semanticError = errorMessage
