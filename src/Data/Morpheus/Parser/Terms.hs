{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Terms
  ( onType
  , spreadLiteral
  , nonNUll
  , charSpace
  , parseTuple
  ) where

import           Control.Applicative               ((<|>))
import           Data.Attoparsec.Text              (Parser, anyChar, char, sepBy, skipSpace, string)
import           Data.Functor                      (($>))
import           Data.Morpheus.Parser.Primitive    (getPosition, token)
import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))
import           Data.Text                         (Text)

nonNUll :: Parser [DataTypeWrapper]
nonNUll = (char '!' $> [NonNullType]) <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser = do
  skipSpace
  parseChar '('
  skipSpace
  parameters <- parser `sepBy` (skipSpace *> char ',')
  skipSpace
  parseChar ')'
  pure parameters

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
