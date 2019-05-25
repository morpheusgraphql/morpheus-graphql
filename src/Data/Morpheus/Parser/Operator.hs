{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Operator
  ( parseAnonymousQuery
  , parseQuery
  , parseSubscription
  , parseMutation
  ) where

import           Control.Applicative                       ((<|>))
import           Data.Attoparsec.Text                      (Parser, char, sepBy, skipSpace, string, try)
import           Data.Morpheus.Parser.Body                 (entries)
import           Data.Morpheus.Parser.Primitive            (getPosition, token, variable)
import           Data.Morpheus.Parser.Terms                (nonNUll)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..), RawOperator, RawOperator',
                                                            Variable (..), VariableDefinitions)
import           Data.Morpheus.Types.Internal.Data         (DataTypeWrapper (..))
import           Data.Text                                 (Text)

wrapMock :: Parser ([DataTypeWrapper], Text)
wrapMock = skipSpace >> token >>= \x -> pure ([], x)

insideList :: Parser ([DataTypeWrapper], Text)
insideList = do
  skipSpace
  _ <- char '['
  skipSpace
  (list, name) <- try wrapMock <|> insideList
  skipSpace
  nonNull' <- nonNUll
  skipSpace
  _ <- char ']'
  return ((ListType : nonNull') ++ list, name)

wrapped :: Parser ([DataTypeWrapper], Text)
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
        , isVariableRequired = 0 < length nonNull'
        , variableTypeWrappers = nonNull' ++ wrappers'
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

parseOperator :: Text -> Parser RawOperator'
parseOperator name' = do
  skipSpace
  pos <- getPosition
  (name, variables) <- operatorHead name'
  skipSpace
  sel <- entries
  pure (Operator' name variables sel pos)

parseAnonymousQuery :: Parser RawOperator
parseAnonymousQuery = do
  skipSpace
  position' <- getPosition
  selection' <- entries
  pure $ Query (Operator' "" [] selection' position')

parseQuery :: Parser RawOperator
parseQuery = Query <$> parseOperator "query"

parseMutation :: Parser RawOperator
parseMutation = Mutation <$> parseOperator "mutation"

parseSubscription :: Parser RawOperator
parseSubscription = Subscription <$> parseOperator "subscription"
