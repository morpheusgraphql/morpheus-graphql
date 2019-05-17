module Data.Morpheus.Parser.Operator
  ( operatorHead
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, char, sepBy, skipSpace, string, try)
import           Data.Morpheus.Parser.Primitive     (getPosition, token, variable)
import           Data.Morpheus.Types.Query.Operator (TypeWrappers (..), Variable (..), VariableDefinitions)
import           Data.Text                          (Text)

nonNUll :: Parser [TypeWrappers]
nonNUll = do
  skipSpace
  _ <- char '!'
  return [NON_NULL]

wrapped :: Parser ([TypeWrappers], Text)
wrapped = do
  skipSpace
  variableType <- token
  nonNull' <- nonNUll
  return (nonNull', variableType)

operatorArgument :: Parser (Text, Variable)
operatorArgument = do
  skipSpace
  pos <- getPosition
  variableName <- variable
  skipSpace
  _ <- char ':'
  (wrappers', type') <- wrapped
  pure (variableName, Variable wrappers' type' pos)

operatorArguments :: Parser VariableDefinitions
operatorArguments = do
  skipSpace
  _ <- char '('
  skipSpace
  parameters <- operatorArgument `sepBy` (skipSpace *> char ',')
  skipSpace
  _ <- char ')'
  pure parameters

operatorHead :: Text -> Parser (Text, VariableDefinitions)
operatorHead kind' = do
  _ <- string kind'
  _ <- char ' '
  skipSpace
  queryName <- token
  variables <- try (skipSpace *> operatorArguments) <|> pure []
  pure (queryName, variables)
