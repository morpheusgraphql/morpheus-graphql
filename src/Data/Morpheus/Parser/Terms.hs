{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Terms
  ( onType
  , spreadLiteral
  , nonNUll
  , charSpace
  , parseMaybeTuple
  , parseTuple
  ) where

import           Control.Applicative               ((<|>))
import           Data.Attoparsec.Combinator        (lookAhead)
import           Data.Attoparsec.Text              (Parser, anyChar, char, sepBy, skipSpace, string)
import           Data.Functor                      (($>))
import           Data.Morpheus.Parser.Primitive    (getPosition, token)
import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))
import           Data.Text                         (Text)

nonNUll :: Parser [DataTypeWrapper]
nonNUll = (char '!' $> [NonNullType]) <|> pure []

parseWhenChar :: Char -> Parser a -> Parser a -> Parser a
parseWhenChar char' parser1 parser2 = do
  x <- lookAhead (skipSpace >> anyChar)
  if x == char'
    then parser1
    else parser2

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseWhenChar '(' (parseTuple parser) (pure [])

parseTuple :: Parser a -> Parser [a]
parseTuple parser = do
  skipSpace
  parseChar '('
  skipSpace
  values <- parser `sepBy` (skipSpace *> char ',')
  skipSpace
  parseChar ')'
  return values

charSpace :: Parser ()
charSpace = parseChar ' '

parseChar :: Char -> Parser ()
parseChar char' = do
  x <- anyChar
  if x == char'
    then return ()
    else fail ("expected '" ++ [char'] ++ "' found '" ++ [x] ++ "'")

onType :: Parser Text
onType = do
  skipSpace
  _ <- string "on "
  skipSpace
  token

spreadLiteral :: Parser Int
spreadLiteral = do
  skipSpace
  index <- getPosition
  _ <- string "..."
  return index
