module Data.Morpheus.Parser.Operator
  ( operatorHead
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, char, sepBy, skipSpace, string, try)
import           Data.Morpheus.Parser.Primitive     (nonNUll, token, variable)
import           Data.Morpheus.Types.Query.Operator (ListWrapper (..), Variable (..), VariableDefinitions)
import           Data.Text                          (Text)

wrapMock :: Parser ([ListWrapper], Text)
wrapMock = skipSpace >> token >>= \x -> pure ([], x)

insideList :: Parser ([ListWrapper], Text)
insideList = do
  skipSpace
  _ <- char '['
  skipSpace
  (list, name) <- try wrapMock <|> insideList
  skipSpace
  nonNull' <- nonNUll
  skipSpace
  _ <- char ']'
  return (ListWrapper nonNull' : list, name)

wrapped :: Parser ([ListWrapper], Text)
wrapped = try insideList <|> wrapMock

operatorArgument :: Parser (Text, Variable)
operatorArgument = do
  skipSpace
  (name', position') <- variable
  skipSpace
  _ <- char ':'
  (wrappers', type') <- wrapped
  nonNull' <- nonNUll
  pure
    ( name'
    , Variable
        { variableType = type'
        , isVariableRequired = nonNull'
        , variableTypeWrappers = wrappers'
        , variablePosition = position'
        })

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
