module Data.Morpheus.Parser.Arguments
  ( arguments
  ) where

import           Control.Applicative            (many, some, (<|>))
import           Data.Attoparsec.Text           (IResult (Done), Parser, char,
                                                 endOfInput, letter, parse,
                                                 parseOnly, sepBy, skipSpace,
                                                 string, try)
import           Data.Map                       (fromList)
import           Data.Morpheus.Parser.Primitive (getPosition, jsBool, jsInt,
                                                 jsString, token, variable)
import           Data.Morpheus.Types.JSType     (JSType (JSEnum))
import           Data.Morpheus.Types.Types      (Argument (..), Arguments)
import           Data.Text                      (Text (..), pack, unpack)

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
