{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Internal
  ( GQLSyntax(..)
  , endParsing
  , gqlSyntax
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text           (Parser, endOfInput, skipSpace)
import           Data.Morpheus.Parser.Primitive (getPosition)
import           Data.Text                      (Text)
import qualified Data.Text                      as T (concat)

data GQLSyntax a
  = Invalid Text
            Int
  | Valid a

gqlSyntax :: Parser a -> Text -> Parser (GQLSyntax a)
gqlSyntax parser message' = do
  position' <- getPosition
  Valid <$> parser <|> return (Invalid (T.concat ["GQL Syntax:", message']) position')

endParsing :: a -> Parser (GQLSyntax a)
endParsing x = do
  skipSpace
  position' <- getPosition
  (endOfInput >> return (Valid x)) <|> return (Invalid "Could not read after the Position" position')
