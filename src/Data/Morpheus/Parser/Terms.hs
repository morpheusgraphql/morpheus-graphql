{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Terms
  ( onType
  , spreadLiteral
  , nonNUll
  ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.Text             (Parser, char, skipSpace, string)
import           Data.Functor                     (($>))
import           Data.Morpheus.Parser.Primitive   (getPosition, token)
import           Data.Morpheus.Types.Internal.AST (ASTTypeWrapper (..))
import           Data.Text                        (Text)

nonNUll :: Parser [ASTTypeWrapper]
nonNUll = (char '!' $> [NonNullType]) <|> pure []

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
