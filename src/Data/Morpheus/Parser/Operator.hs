module Data.Morpheus.Parser.Operator
  ( operatorArguments
  ) where

import           Data.Attoparsec.Text               (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.Primitive     (getPosition, token, variable)
import           Data.Morpheus.Types.Query.Operator (Variable (..), VariableDefinitions)
import           Data.Text                          (Text)

rootHeadVariable :: Parser (Text, Variable)
rootHeadVariable = do
  skipSpace
  pos <- getPosition
  variableName <- variable
  skipSpace
  _ <- char ':'
  skipSpace
  variableType <- token
  pure (variableName, Variable variableType pos)

operatorArguments :: Parser VariableDefinitions
operatorArguments = do
  skipSpace
  _ <- char '('
  skipSpace
  parameters <- rootHeadVariable `sepBy` (skipSpace *> char ',')
  skipSpace
  _ <- char ')'
  pure parameters
