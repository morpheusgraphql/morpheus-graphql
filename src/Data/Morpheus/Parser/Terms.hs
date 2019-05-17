{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Terms
  ( onType
  , spreadLiteral
  ) where

import           Data.Attoparsec.Text           (Parser, skipSpace, string)
import           Data.Morpheus.Parser.Primitive (getPosition, token)
import           Data.Text                      (Text)

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
