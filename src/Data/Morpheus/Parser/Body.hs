{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Control.Applicative                    ((<|>))
import           Data.Attoparsec.Text                   (Parser, char, sepBy, skipSpace, string, try)
import           Data.Morpheus.Parser.Arguments         (arguments)
import           Data.Morpheus.Parser.Primitive         (getPosition, separator, token)
import           Data.Morpheus.Types.Query.RawSelection (RawArguments, RawSelection (..), RawSelectionSet)
import           Data.Text                              (Text)

spreadLiteral :: Parser Int
spreadLiteral = do
  skipSpace
  index <- getPosition
  _ <- string "..."
  return index

spread :: Parser (Text, RawSelection)
spread = do
  index <- spreadLiteral
  skipSpace
  key' <- token
  return (key', Spread key' index)

inlineFragment :: Parser (Text, RawSelection)
inlineFragment = do
  index <- spreadLiteral
  skipSpace
  _ <- string "on "
  skipSpace
  onType' <- token
  skipSpace
  fragmentBody <- entries
  pure ("INLINE_FRAGMENT", InlineFragment onType' fragmentBody index)

entry :: Parser (Text, RawSelection)
entry = do
  skipSpace
  index <- getPosition
  key <- token
  args <- try arguments <|> pure []
  value <- try (body args) <|> pure (RawField args key index)
  return (key, value)

separated :: Parser a -> Parser [a]
separated x = x `sepBy` separator

entries :: Parser RawSelectionSet
entries = do
  _ <- char '{'
  skipSpace
  entries' <- separated (entry <|> inlineFragment <|> spread)
  skipSpace
  _ <- char '}'
  return entries'

body :: RawArguments -> Parser RawSelection
body args = do
  skipSpace
  index <- getPosition
  entries' <- entries
  return (RawSelectionSet args entries' index)
