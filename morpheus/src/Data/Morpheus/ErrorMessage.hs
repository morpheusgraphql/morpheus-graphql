{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.ErrorMessage
  ( syntaxError
  , cannotQueryField
  , subfieldsNotSelected
  , handleError
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
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..)
                                                , LineMarks
                                                , Position
                                                )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                , GQLErrors
                                                )
import           Data.Data                      ( dataTypeOf
                                                , dataTypeName
                                                , Data
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.Error.Utils      ( errorMessage )


handleError x = Left $ errorMessage [] 0 $ T.concat ["Field Error: ", x]

invalidEnumOption :: LineMarks -> MetaInfo -> GQLErrors
invalidEnumOption lines meta = errorMessage lines (position meta) text
 where
  text = T.concat
    ["Invalid Option \"", key meta, "\" on Enum \"", typeName meta, "\"."]

unsupportedArgumentType :: LineMarks -> MetaInfo -> GQLErrors
unsupportedArgumentType lines meta = errorMessage lines (position meta) text
 where
  text = T.concat
    ["Argument \"", key meta, "\" has unsuported type \"", typeName meta, "\"."]

variableIsNotDefined :: LineMarks -> MetaInfo -> GQLErrors
variableIsNotDefined lines meta = errorMessage lines (position meta) text
 where
  text = T.concat
    [ "Variable \""
    , key meta
    , "\" is not defined by operation \""
    , typeName meta
    , "\"."
    ]

unknownArguments :: Text -> [Text] -> GQLErrors
unknownArguments fieldName = map keyToError
 where
  keyToError x = GQLError { desc = toMessage x, posIndex = 0 }
  toMessage key =
    T.concat ["Unknown Argument \"", key, "\" on Field \"", fieldName, "\"."]

requiredArgument :: LineMarks -> MetaInfo -> GQLErrors
requiredArgument lines meta = errorMessage lines (position meta) text
 where
  text = T.concat
    [ "Required Argument: \""
    , key meta
    , "\" not Found on type \""
    , typeName meta
    , "\"."
    ]

fieldTypeMismatch :: LineMarks -> MetaInfo -> JSType -> Text -> GQLErrors
fieldTypeMismatch lines meta isType should = errorMessage lines
                                                          (position meta)
                                                          text
 where
  text = T.concat
    [ "field \""
    , key meta
    , "\"on type \""
    , typeName meta
    , "\" has a type \""
    , pack $ show isType
    , "\" but should have \""
    , should
    , "\"."
    ]

cannotQueryField :: LineMarks -> MetaInfo -> GQLErrors
cannotQueryField lines meta = errorMessage lines (position meta) text
 where
  text = T.concat
    ["Cannot query field \"", key meta, "\" on type \"", typeName meta, "\"."]

subfieldsNotSelected :: LineMarks -> MetaInfo -> GQLErrors
subfieldsNotSelected lines meta = errorMessage lines (position meta) text
 where
  text = T.concat
    [ "Field \""
    , key meta
    , "\" of type \""
    , typeName meta
    , "\" must have a selection of subfields"
    ]

syntaxError :: Text -> LineMarks -> Position -> GQLErrors
syntaxError e lines pos =
  errorMessage lines pos $ T.concat ["Syntax Error: ", e]
