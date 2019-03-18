module Data.Morpheus.Parser.RootHead
  ( rootHeadArguments
  ) where

import           Data.Attoparsec.Text                   (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.Primitive         (getPosition, token, variable)
import           Data.Morpheus.Types.Query.RawSelection (RawArgument (..), RawArguments)
import           Data.Text                              (Text)

rootHeadVariable :: Parser (Text, RawArgument)
rootHeadVariable = do
  skipSpace
  pos <- getPosition
  variableName <- variable
  skipSpace
  _ <- char ':'
  skipSpace
  variableType <- token
  pure (variableName, Variable variableType pos)

rootHeadArguments :: Parser RawArguments
rootHeadArguments = do
  skipSpace
  _ <- char '('
  skipSpace
  parameters <- rootHeadVariable `sepBy` (skipSpace *> char ',')
  skipSpace
  _ <- char ')'
  pure parameters
