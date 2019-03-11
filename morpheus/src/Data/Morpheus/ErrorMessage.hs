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
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLError (..), GQLErrors)
import           Data.Morpheus.Types.JSType   (JSType (..))
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..), Position)
import           Data.Text                    (Text, pack)
import qualified Data.Text                    as T (concat)

handleError x = Left $ errorMessage 0 $ T.concat ["Field Error: ", x]

invalidEnumOption :: MetaInfo -> GQLErrors
invalidEnumOption meta = errorMessage (position meta) text
  where
    text = T.concat ["Invalid Option \"", key meta, "\" on Enum \"", typeName meta, "\"."]

unsupportedArgumentType :: MetaInfo -> GQLErrors
unsupportedArgumentType meta = errorMessage (position meta) text
  where
    text = T.concat ["Argument \"", key meta, "\" has unsuported type \"", typeName meta, "\"."]

variableIsNotDefined :: MetaInfo -> GQLErrors
variableIsNotDefined meta = errorMessage (position meta) text
  where
    text = T.concat ["Variable \"", key meta, "\" is not defined by operation \"", typeName meta, "\"."]

unknownArguments :: Text -> [Text] -> GQLErrors
unknownArguments fieldName = map keyToError
  where
    keyToError x = GQLError {desc = toMessage x, posIndex = 0}
    toMessage key = T.concat ["Unknown Argument \"", key, "\" on Field \"", fieldName, "\"."]

requiredArgument :: MetaInfo -> GQLErrors
requiredArgument meta = errorMessage (position meta) text
  where
    text = T.concat ["Required Argument: \"", key meta, "\" not Found on type \"", typeName meta, "\"."]

fieldTypeMismatch :: MetaInfo -> JSType -> Text -> GQLErrors
fieldTypeMismatch meta isType should = errorMessage (position meta) text
  where
    text =
      T.concat
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

cannotQueryField :: MetaInfo -> GQLErrors
cannotQueryField meta = errorMessage (position meta) text
  where
    text = T.concat ["Cannot query field \"", key meta, "\" on type \"", typeName meta, "\"."]

subfieldsNotSelected :: MetaInfo -> GQLErrors
subfieldsNotSelected meta = errorMessage (position meta) text
  where
    text = T.concat ["Field \"", key meta, "\" of type \"", typeName meta, "\" must have a selection of subfields"]

syntaxError :: Text -> Position -> GQLErrors
syntaxError e pos = errorMessage pos $ T.concat ["Syntax Error: ", e]
