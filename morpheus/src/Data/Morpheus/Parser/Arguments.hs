module Data.Morpheus.Parser.Arguments
  ( arguments
  ) where

import           Control.Applicative                    ((<|>))
import           Data.Attoparsec.Text                   (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.Primitive         (getPosition, jsBool, jsInt, jsString, token, variable)
import           Data.Morpheus.Types.JSType             (JSType (JSEnum))
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..), RawArguments)
import           Data.Text                              (Text)

enum :: Parser JSType
enum = JSEnum <$> token

argumentType :: Parser RawArgument
argumentType = do
  pos <- getPosition
  arg <- jsString <|> jsInt <|> jsBool <|> enum
  pure $ Argument arg pos

variableType :: Parser RawArgument
variableType = do
  pos <- getPosition
  val <- variable
  pure $ VariableReference val pos

inputValue :: Parser RawArgument
inputValue = skipSpace *> argumentType <|> variableType

parameter :: Parser (Text, RawArgument)
parameter = do
  skipSpace
  key <- token
  skipSpace
  _ <- char ':'
  skipSpace
  value <- inputValue
  pure (key, value)

arguments :: Parser RawArguments
arguments = do
  skipSpace
  _ <- char '('
  skipSpace
  parameters <- parameter `sepBy` (skipSpace *> char ',')
  skipSpace
  _ <- char ')'
  pure parameters
