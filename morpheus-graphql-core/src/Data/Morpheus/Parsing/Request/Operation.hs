{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Parsing.Request.Operation
  ( parseOperation,
  )
where

import Data.Functor (($>))
--
-- MORPHEUS

import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Pattern
  ( optionalDirectives,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( operator,
    parseName,
    parseType,
    spaceAndComments1,
    uniqTupleOpt,
    variable,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( parseDefaultValue,
  )
import Data.Morpheus.Parsing.Request.Selection
  ( parseSelectionSet,
  )
import Data.Morpheus.Types.Internal.AST
  ( Operation (..),
    OperationType (..),
    RAW,
    Ref (..),
    Variable (..),
    VariableContent (..),
  )
import Text.Megaparsec
  ( (<?>),
    (<|>),
    label,
    optional,
  )
import Text.Megaparsec.Char (string)

-- Variables :  https://graphql.github.io/graphql-spec/June2018/#VariableDefinition
--
--  VariableDefinition
--    Variable : Type DefaultValue(opt)
--
variableDefinition :: Parser (Variable RAW)
variableDefinition = label "VariableDefinition" $ do
  (Ref variableName variablePosition) <- variable
  operator ':'
  variableType <- parseType
  variableValue <- DefaultValue <$> optional parseDefaultValue
  pure Variable {..}

-- Operations : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Operations
--
-- OperationDefinition
--   OperationType Name(opt) VariableDefinitions(opt) Directives(opt) SelectionSet
--
--   OperationType: one of
--     query, mutation,    subscription
parseOperationDefinition :: Parser (Operation RAW)
parseOperationDefinition = label "OperationDefinition" $ do
  operationPosition <- getLocation
  operationType <- parseOperationType
  operationName <- optional parseName
  operationArguments <- uniqTupleOpt variableDefinition
  operationDirectives <- optionalDirectives
  operationSelection <- parseSelectionSet
  pure Operation {..}

parseOperationType :: Parser OperationType
parseOperationType = label "OperationType" $ do
  kind <-
    (string "query" $> Query)
      <|> (string "mutation" $> Mutation)
      <|> (string "subscription" $> Subscription)
  spaceAndComments1
  return kind

parseAnonymousQuery :: Parser (Operation RAW)
parseAnonymousQuery = label "AnonymousQuery" $ do
  operationPosition <- getLocation
  operationSelection <- parseSelectionSet
  pure
    ( Operation
        { operationName = Nothing,
          operationType = Query,
          operationArguments = empty,
          operationDirectives = empty,
          ..
        }
    )
    <?> "can't parse AnonymousQuery"

parseOperation :: Parser (Operation RAW)
parseOperation = parseAnonymousQuery <|> parseOperationDefinition
