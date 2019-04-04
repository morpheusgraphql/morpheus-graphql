{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Arguments
  ( requiredArgument
  , unknownArguments
  , argumentError
  , argumentGotInvalidValue
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Core     (EnhancedKey (..))
import           Data.Morpheus.Types.Error    (GQLError (..), GQLErrors, MetaError (..))
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..))
import           Data.Text                    (Text)
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
argumentError :: MetaError -> GQLErrors
argumentError (UnknownType meta)    = requiredArgument meta
argumentError (UnknownField meta)   = requiredArgument meta
argumentError (TypeMismatch meta _) = requiredArgument meta

argumentGotInvalidValue :: Text -> Text -> Int -> GQLErrors
argumentGotInvalidValue name' inputMessage' position' = errorMessage position' text
  where
    text = T.concat ["Argument \"", name', "\" got invalid value ;", inputMessage']


unknownArguments :: Text -> [EnhancedKey] -> GQLErrors
unknownArguments fieldName = map keyToError
  where
    keyToError (EnhancedKey argName pos) = GQLError {desc = toMessage argName, posIndex = [pos]}
    toMessage argName = T.concat ["Unknown Argument \"", argName, "\" on Field \"", fieldName, "\"."]

requiredArgument :: MetaInfo -> GQLErrors
requiredArgument meta = errorMessage (position meta) text
  where
    text = T.concat ["Required Argument: \"", key meta, "\" not Found on type \"", typeName meta, "\"."]
