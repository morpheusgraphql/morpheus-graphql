{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Terms
  ( onType
  , spreadLiteral
  , nonNUll
  , charSpace
  , parseMaybeTuple
  ) where

import           Control.Applicative               ((<|>))
import           Data.Attoparsec.Text              (Parser, anyChar, char, sepBy, skipSpace, string)
import           Data.Functor                      (($>))
import           Data.Morpheus.Parser.Primitive    (getPosition, token)
import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))
import           Data.Text                         (Text)

nonNUll :: Parser [DataTypeWrapper]
nonNUll = (char '!' $> [NonNullType]) <|> pure []

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = do
  skipSpace
  x <- anyChar
  if x == '('
    then parseTuple
    else pure []
  where
    parseTuple = do
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
