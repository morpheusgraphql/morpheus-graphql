{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.ErrorMessage
  ( syntaxError
  , cannotQueryField
  , subfieldsNotSelected
  , handleError
  , requiredArgument
  , errorMessage
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

{-
  ARGUMENTS:
    type Experience {
        experience ( lang: LANGUAGE ) : String ,
        date: String
    }

  - required field !?
  - experience( lang: "bal" ) -> "Expected type LANGUAGE, found \"a\"."
  - experience( lang: Bla ) -> "Expected type LANGUAGE, found Bla."
  - experience( lang: 1 ) -> "Expected type LANGUAGE, found 1."
  - experience( a1 : 1 ) -> "Unknown argument \"a1\" on field \"experience\" of type \"Experience\".",
  - date(name: "name") -> "Unknown argument \"name\" on field \"date\" of type \"Experience\"."
-}


{-
  FRAGMENT:
    type Experience {
        experience ( lang: LANGUAGE ) : String ,
        date: String
    }
    fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
    fragment H on T1 { ...A} , fragment A on T { ...H } -> "Cannot spread fragment \"H\" within itself via A."
    fragment H on D {...}  ->  "Unknown type \"D\"."
-}


-- GQL:: if no mutation defined -> "Schema is not configured for mutations."

handleError :: Text -> Either GQLErrors b
handleError x = Left $ errorMessage 0 $ T.concat ["Field Error: ", x]

invalidEnumOption :: MetaInfo -> GQLErrors
invalidEnumOption meta = errorMessage (position meta) text
  where
    text = T.concat ["Expected type ", typeName meta, " found ", key meta, "."]



unsupportedArgumentType :: MetaInfo -> GQLErrors
unsupportedArgumentType meta = errorMessage (position meta) text
  where
    text = T.concat ["Argument \"", key meta, "\" has unsuported type \"", typeName meta, "\"."]


unknownArguments :: Text -> [Text] -> GQLErrors
unknownArguments fieldName = map keyToError
  where
    keyToError x = GQLError {desc = toMessage x, posIndex = 0}
    toMessage argName = T.concat ["Unknown Argument \"", argName, "\" on Field \"", fieldName, "\"."]

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

-- GQL: "Field \"default\" must not have a selection since type \"String!\" has no subfields."

cannotQueryField :: MetaInfo -> GQLErrors
cannotQueryField meta = errorMessage (position meta) text
  where
    text = T.concat ["Cannot query field \"", key meta, "\" on type \"", typeName meta, "\"."]

-- GQL:: Field \"hobby\" of type \"Hobby!\" must have a selection of subfields. Did you mean \"hobby { ... }\"?
subfieldsNotSelected :: MetaInfo -> GQLErrors
subfieldsNotSelected meta = errorMessage (position meta) text
  where
    text = T.concat ["Field \"", key meta, "\" of type \"", typeName meta, "\" must have a selection of subfields"]

syntaxError :: Text -> Position -> GQLErrors
syntaxError e pos = errorMessage pos $ T.concat ["Syntax Error: ", e]
