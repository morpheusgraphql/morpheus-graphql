{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Arguments
  ( undefinedArgument
  , unknownArguments
  , argumentGotInvalidValue
  , argumentNameCollision
  )
where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Position
                                                )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( GQLError(..)
                                                , GQLErrors
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
                                                ( concat )

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
argumentGotInvalidValue :: Text -> Text -> Position -> GQLErrors
argumentGotInvalidValue key' inputMessage' position' = errorMessage position'
                                                                    text
 where
  text = T.concat ["Argument ", key', " got invalid value ;", inputMessage']

unknownArguments :: Text -> [Ref] -> GQLErrors
unknownArguments fieldName = map keyToError
 where
  keyToError (Ref argName pos) =
    GQLError { message = toMessage argName, locations = [pos] }
  toMessage argName = T.concat
    ["Unknown Argument \"", argName, "\" on Field \"", fieldName, "\"."]

argumentNameCollision :: [Ref] -> GQLErrors
argumentNameCollision = map keyToError
 where
  keyToError (Ref argName pos) =
    GQLError { message = toMessage argName, locations = [pos] }
  toMessage argName =
    T.concat ["There can Be only One Argument Named \"", argName, "\""]

undefinedArgument :: Ref -> GQLErrors
undefinedArgument (Ref key' position') = errorMessage position' text
  where text = T.concat ["Required Argument: \"", key', "\" was not Defined"]
