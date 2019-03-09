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
                                                , GQLErrors
                                                , Position(..)
                                                )
import           Data.Data                      ( dataTypeOf
                                                , dataTypeName
                                                , Data
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.Error.Utils      (errorMessage)


handleError x = Left $ errorMessage $ T.concat ["Field Error: ", x]

invalidEnumOption :: MetaInfo -> GQLErrors
invalidEnumOption line meta = errorMessage [] $ T.concat
    ["Invalid Option \"", key meta, "\" on Enum \"", className meta, "\"."]

unsupportedArgumentType :: MetaInfo -> GQLErrors
unsupportedArgumentType meta = errorMessage $ T.concat
    [ "Argument \""
    , key meta
    , "\" has unsuported type \""
    , className meta
    , "\"."
    ]

variableIsNotDefined :: MetaInfo -> GQLErrors
variableIsNotDefined meta = errorMessage $ T.concat
    [ "Variable \""
    , key meta
    , "\" is not defined by operation \""
    , className meta
    , "\"."
    ]

unknownArguments :: Text -> [Text] -> GQLErrors
unknownArguments fieldName = map keyToError
  where
    keyToError x =
        GQLError { message = toMessage x, locations = [ErrorLocation 0 0] }
    toMessage key = T.concat
        ["Unknown Argument \"", key, "\" on Field \"", fieldName, "\"."]

requiredArgument :: MetaInfo -> GQLErrors
requiredArgument meta = errorMessage $ T.concat
    [ "Required Argument: \""
    , key meta
    , "\" not Found on type \""
    , className meta
    , "\"."
    ]

fieldTypeMismatch :: MetaInfo -> JSType -> Text -> GQLErrors
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

cannotQueryField :: MetaInfo -> GQLErrors
cannotQueryField meta = errorMessage $ T.concat
    ["Cannot query field \"", key meta, "\" on type \"", className meta, "\"."]

subfieldsNotSelected :: MetaInfo -> GQLErrors
subfieldsNotSelected meta = errorMessage $ T.concat
    [ "Field \""
    , key meta
    , "\" of type \""
    , className meta
    , "\" must have a selection of subfields"
    ]

syntaxError :: Text -> GQLErrors
syntaxError e = errorMessage $ T.concat ["Syntax Error: ", e]

semanticError :: Text -> GQLErrors
semanticError = errorMessage
