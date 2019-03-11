module Data.Morpheus.Parser.RootHead
  ( rootHeadArguments
  ) where

import           Data.Attoparsec.Text           (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.Primitive (getPosition, token, variable)
import           Data.Morpheus.Types.Types      (Argument (..), Arguments)
import           Data.Text                      (Text)

rootHeadVariable :: Parser (Text, Argument)
rootHeadVariable = do
  skipSpace
  pos <- getPosition
  variableName <- variable
  skipSpace
  char ':'
  skipSpace
  variableType <- token
  pure (variableName, Variable variableType pos)

rootHeadArguments :: Parser Arguments
rootHeadArguments = do
  skipSpace
  char '('
  skipSpace
  parameters <- rootHeadVariable `sepBy` (skipSpace *> char ',')
  skipSpace
  char ')'
  pure parameters
