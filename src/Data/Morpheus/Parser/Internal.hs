{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Internal
  ( GQLSyntax(..)
  , endParsing
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text           (Parser, endOfInput, skipSpace)
import           Data.Morpheus.Parser.Primitive (getPosition)
import           Data.Text                      (Text)

data GQLSyntax a
  = Invalid Text
            Int
  | Valid a

endParsing :: a -> Parser (GQLSyntax a)
endParsing x = do
  skipSpace
  position' <- getPosition
  (endOfInput >> return (Valid x)) <|> return (Invalid "Could not read after the Position" position')
