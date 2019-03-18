{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Control.Applicative                    (some, (<|>))
import           Data.Attoparsec.Text                   (Parser, char, letter, sepBy, skipSpace,
                                                         string, try)
import           Data.Morpheus.Parser.Arguments         (arguments)
import           Data.Morpheus.Parser.Primitive         (getPosition, separator, token)
import           Data.Morpheus.Types.Query.RawSelection (RawArguments, RawSelection (..),
                                                         RawSelectionSet)
import           Data.Text                              (Text, pack)

spread :: Parser (Text, RawSelection)
spread = do
  skipSpace
  index <- getPosition
  _ <- string "..."
  key <- some (letter <|> char '_')
  return (pack key, Spread (pack key) index)

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
  entries' <- separated $ entry <|> spread
  skipSpace
  _ <- char '}'
  return entries'

body :: RawArguments -> Parser RawSelection
body args = do
  skipSpace
  index <- getPosition
  entries' <- entries
  return (RawSelectionSet args entries' index)
