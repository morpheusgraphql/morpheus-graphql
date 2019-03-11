module Data.Morpheus.Parser.Arguments
  ( arguments
  ) where

import           Control.Applicative            ((<|>))
import           Data.Attoparsec.Text           (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.Primitive (getPosition, jsBool, jsInt,
                                                 jsString, token, variable)
import           Data.Morpheus.Types.JSType     (JSType (JSEnum))
import           Data.Morpheus.Types.Types      (Argument (..), Arguments)
import           Data.Text                      (Text)

enum = JSEnum <$> token

argumentType :: Parser Argument
argumentType = do
  pos <- getPosition
  arg <- enum <|> jsString <|> jsBool <|> jsInt
  pure $ Argument arg pos

variableType :: Parser Argument
variableType = do
  pos <- getPosition
  val <- variable
  pure $ Variable val pos

inputValue :: Parser Argument
inputValue = skipSpace *> argumentType <|> variableType

parameter :: Parser (Text, Argument)
parameter = do
  skipSpace
  key <- token
  skipSpace
  char ':'
  skipSpace
  value <- inputValue
  pure (key, value)

arguments :: Parser Arguments
arguments = do
  skipSpace
  char '('
  skipSpace
  parameters <- parameter `sepBy` (skipSpace *> char ',')
  skipSpace
  char ')'
  pure parameters
