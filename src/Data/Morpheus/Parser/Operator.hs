{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Operator
  ( parseAnonymousQuery
  , parseOperator
  ) where

import           Control.Applicative                       ((<|>))
import           Data.Attoparsec.Text                      (Parser, char, skipSpace, string, try, (<?>))
import           Data.Functor                              (($>))
import           Data.Morpheus.Parser.Body                 (entries)
import           Data.Morpheus.Parser.Primitive            (getPosition, token, variable)
import           Data.Morpheus.Parser.Terms                (charSpace, nonNUll, parseAssignment, parseMaybeTuple)
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

wrappedSignature :: Parser ([DataTypeWrapper], Text)
wrappedSignature = try insideList <|> wrapMock

operatorArgument :: Parser (Text, Variable)
operatorArgument = do
  ((name', position'), (wrappers', type')) <- parseAssignment variable wrappedSignature
  nonNull' <- nonNUll
  pure
    ( name'
    , Variable
        { variableType = type'
        , isVariableRequired = 0 < length nonNull'
        , variableTypeWrappers = nonNull' ++ wrappers'
        , variablePosition = position'
        })

operatorHead :: Parser (RawOperator' -> RawOperator, Text, VariableDefinitions)
operatorHead = do
  wrapper' <- operatorKind
  charSpace
  skipSpace
  queryName <- token
  variables <- parseMaybeTuple operatorArgument
  pure (wrapper', queryName, variables)

parseOperator :: Parser RawOperator
parseOperator = do
  skipSpace
  pos <- getPosition
  (wrapper', name, variables) <- operatorHead
  skipSpace
  sel <- entries
  pure (wrapper' $ Operator' name variables sel pos)

parseAnonymousQuery :: Parser RawOperator
parseAnonymousQuery = do
  skipSpace
  position' <- getPosition
  selection' <- entries
  pure (Query $ Operator' "" [] selection' position') <?> "can't parse AnonymousQuery"

operatorKind :: Parser (RawOperator' -> RawOperator)
operatorKind = (string "query" $> Query) <|> (string "mutation" $> Mutation) <|> (string "subscription" $> Subscription)
