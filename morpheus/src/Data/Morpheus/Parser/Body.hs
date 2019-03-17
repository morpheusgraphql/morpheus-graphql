{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Control.Applicative            (some, (<|>))
import           Data.Attoparsec.Text           (Parser, char, letter, sepBy, skipSpace, string,
                                                 try)
import           Data.Morpheus.Parser.Arguments (arguments)
import           Data.Morpheus.Parser.Primitive (getPosition, separator, token)
import           Data.Morpheus.Types.Types      (Arguments, QuerySelection (..), SelectionSet)
import           Data.Text                      (Text, pack)

spread :: Parser (Text, QuerySelection)
spread = do
  skipSpace
  index <- getPosition
  _ <- string "..."
  key <- some (letter <|> char '_')
  return (pack key, Spread (pack key) index)

entry :: Parser (Text, QuerySelection)
entry = do
  skipSpace
  index <- getPosition
  key <- token
  args <- try arguments <|> pure []
  value <- try (body args) <|> pure (Field args key index)
  return (key, value)

separated :: Parser a -> Parser [a]
separated x = x `sepBy` separator

entries :: Parser SelectionSet
entries = do
  _ <- char '{'
  skipSpace
  entries' <- separated $ entry <|> spread
  skipSpace
  _ <- char '}'
  return entries'

body :: Arguments -> Parser QuerySelection
body args = do
  skipSpace
  index <- getPosition
  entries' <- entries
  return (SelectionSet args entries' index)
