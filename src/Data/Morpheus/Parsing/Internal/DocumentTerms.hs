{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Internal.DocumentTerms
  ( parseMaybeTuple
  , parseAssignment
  , pipe
  , setOf
  ) where

import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (spaceAndComments)
import           Text.Megaparsec                         (between, label, many, sepBy, sepEndBy, (<?>), (<|>))
import           Text.Megaparsec.Char                    (char)

setOf :: Parser a -> Parser [a]
setOf entry = setLiteral (entry `sepEndBy` many (char ',' *> spaceAndComments))

setLiteral :: Parser [a] -> Parser [a]
setLiteral = between (char '{' *> spaceAndComments) (char '}' *> spaceAndComments)

pipe :: Parser ()
pipe = char '|' *> spaceAndComments

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
  between
    (char '(' *> spaceAndComments)
    (char ')' *> spaceAndComments)
    (parser `sepBy` (many (char ',') *> spaceAndComments) <?> "empty Tuple value!")

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' =
  label "assignment" $ do
    name' <- nameParser'
    char ':' *> spaceAndComments
    value' <- valueParser'
    pure (name', value')
