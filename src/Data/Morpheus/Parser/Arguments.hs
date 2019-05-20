module Data.Morpheus.Parser.Arguments
  ( arguments
  ) where

import           Control.Applicative                          ((<|>))
import           Data.Attoparsec.Text                         (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.InputValues.InputList   (inputList)
import           Data.Morpheus.Parser.InputValues.InputObject (inputObject)
import           Data.Morpheus.Parser.Primitive               (getPosition, jsBool, jsNumber, jsString, token, variable)
import           Data.Morpheus.Types.Internal.Value           (Value (Enum))
import           Data.Morpheus.Types.Query.RawSelection       (RawArgument (..), RawArguments)
import           Data.Text                                    (Text)

enum :: Parser Value
enum = Enum <$> token

jsValue :: Parser Value
jsValue = jsString <|> jsNumber <|> jsBool <|> inputObject jsValue <|> inputList jsValue

argumentType :: Parser RawArgument
argumentType = do
  pos <- getPosition
  arg <- jsValue <|> enum
  pure $ Argument arg pos

variableType :: Parser RawArgument
variableType = do
  (reference', position') <- variable
  pure $ VariableReference reference' position'

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
