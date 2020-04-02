{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Arguments
  ( undefinedArgument
  , argumentGotInvalidValue
  )
where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Position
                                                , GQLError(..)
                                                , GQLErrors
                                                , Message
                                                , Name
                                                )
                                              
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
argumentGotInvalidValue :: Name -> Message
argumentGotInvalidValue argName = "Argument " <> argName <> " got invalid value. "

undefinedArgument :: Ref -> GQLErrors
undefinedArgument (Ref key' position') = errorMessage position' text
  where text = "Required Argument: \"" <> key' <> "\" was not Defined"
