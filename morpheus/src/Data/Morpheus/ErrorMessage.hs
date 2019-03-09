{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.ErrorMessage
    ( syntaxError
    , cannotQueryField
    , subfieldsNotSelected
    , handleError
    , semanticError
    , requiredArgument
    , errorMessage
    , variableIsNotDefined
    , unsupportedArgumentType
    , invalidEnumOption
    , unknownArguments
    , fieldTypeMismatch
    )
where

import qualified Data.Text                     as T
                                                ( concat )
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
import           Data.Morpheus.Types.JSType     ( JSType(..) )


type Errors = [ [Int] ->  GQLError ]


errorMessage :: Text -> Errors
errorMessage x =
    [\_ -> GQLError { message = x, locations = [ErrorLocation 0 0] }]

handleError x = Left $ errorMessage $ T.concat ["Field Error: ", x]

invalidEnumOption :: MetaInfo -> Errors
invalidEnumOption meta = errorMessage $ T.concat
    ["Invalid Option \"", key meta, "\" on Enum \"", className meta, "\"."]

unsupportedArgumentType :: MetaInfo -> Errors
unsupportedArgumentType meta = errorMessage $ T.concat
    [ "Argument \""
    , key meta
    , "\" has unsuported type \""
    , className meta
    , "\"."
    ]

variableIsNotDefined :: MetaInfo -> Errors
variableIsNotDefined meta = errorMessage $ T.concat
    [ "Variable \""
    , key meta
    , "\" is not defined by operation \""
    , className meta
    , "\"."
    ]

unknownArguments :: Text -> [Text] -> Errors
unknownArguments fieldName = map keyToError
  where
    keyToError x _ =
        GQLError { message = toMessage x, locations = [ErrorLocation 0 0] }
    toMessage key = T.concat
        ["Unknown Argument \"", key, "\" on Field \"", fieldName, "\"."]

requiredArgument :: MetaInfo -> Errors
requiredArgument meta = errorMessage $ T.concat
    [ "Required Argument: \""
    , key meta
    , "\" not Found on type \""
    , className meta
    , "\"."
    ]

fieldTypeMismatch :: MetaInfo -> JSType -> Text -> Errors
fieldTypeMismatch meta isType should = errorMessage $ T.concat
    [ "field \""
    , key meta
    , "\"on type \""
    , className meta
    , "\" has a type \""
    , pack $ show isType
    , "\" but should have \""
    , should
    , "\"."
    ]

cannotQueryField :: MetaInfo -> Errors
cannotQueryField meta = errorMessage $ T.concat
    ["Cannot query field \"", key meta, "\" on type \"", className meta, "\"."]

subfieldsNotSelected :: MetaInfo -> Errors
subfieldsNotSelected meta = errorMessage $ T.concat
    [ "Field \""
    , key meta
    , "\" of type \""
    , className meta
    , "\" must have a selection of subfields"
    ]

syntaxError :: Text -> Errors
syntaxError e = errorMessage $ T.concat ["Syntax Error: ", e]

semanticError :: Text -> Errors
semanticError = errorMessage
