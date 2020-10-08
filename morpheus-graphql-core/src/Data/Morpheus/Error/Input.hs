{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Input
  ( typeViolation,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( TypeRef (..),
    ValidationError,
    Value,
    msgValidation,
  )
import Data.Semigroup ((<>))

typeViolation :: TypeRef -> Value s -> ValidationError
typeViolation expected found =
  "Expected type "
    <> msgValidation expected
    <> " found "
    <> msgValidation found
    <> "."

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
