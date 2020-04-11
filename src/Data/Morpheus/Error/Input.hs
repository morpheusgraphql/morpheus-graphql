{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Input
  ( typeViolation 
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.Text                      ( pack )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST 
                                                ( Message
                                                , ResolvedValue
                                                , TypeRef(..) 
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) )


typeViolation :: TypeRef -> ResolvedValue -> Message
typeViolation expected found = "Expected type \""
  <> render expected
  <> "\" found "
  <> pack (unpack $ encode found)
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