{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Terms
  ( onType
  , spreadLiteral
  , nonNUll
  , charSpace
  ) where

import           Control.Applicative               ((<|>))
import           Data.Attoparsec.Text              (Parser, anyChar, char, skipSpace, string)
import           Data.Functor                      (($>))
import           Data.Morpheus.Parser.Primitive    (getPosition, token)
import           Data.Morpheus.Types.Internal.Data (DataTypeWrapper (..))
import           Data.Text                         (Text)

nonNUll :: Parser [DataTypeWrapper]
nonNUll = (char '!' $> [NonNullType]) <|> pure []

charSpace :: Parser ()
charSpace = do
  x <- anyChar
  if x == ' '
    then return ()
    else fail ("expected ' ' found '" ++ [x] ++ "'")

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
