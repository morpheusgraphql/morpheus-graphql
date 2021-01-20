{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Request.Operation
  ( parseOperation,
  )
where

import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Pattern
  ( optionalDirectives,
    parseOperationType,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( Term,
    colon,
    parseName,
    parseType,
    uniqTupleOpt,
    varName,
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
    Variable (..),
    VariableContent (..),
  )
import Relude hiding (empty)
import Text.Megaparsec
  ( (<?>),
    Stream,
    Tokens,
    label,
  )

-- Variables :  https://graphql.github.io/graphql-spec/June2018/#VariableDefinition
--
--  VariableDefinition
--    Variable : Type DefaultValue(opt)
--
variableDefinition :: (Stream s, Term s, IsString s, IsString (Tokens s)) => Parser s (Variable RAW)
variableDefinition =
  label "VariableDefinition" $
    Variable
      <$> getLocation
      <*> (varName <* colon)
      <*> parseType
      <*> (DefaultValue <$> optional parseDefaultValue)

-- Operations : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Operations
--
-- OperationDefinition
--   OperationType Name(opt) VariableDefinitions(opt) Directives(opt) SelectionSet
--
--   OperationType: one of
--     query, mutation,    subscription
parseOperationDefinition :: (Stream s, Term s, IsString s, IsString (Tokens s)) => Parser s (Operation RAW)
parseOperationDefinition =
  label "OperationDefinition" $
    Operation
      <$> getLocation
      <*> parseOperationType
      <*> optional parseName
      <*> uniqTupleOpt variableDefinition
      <*> optionalDirectives
      <*> parseSelectionSet

parseAnonymousQuery :: (Stream s, Term s, IsString s, IsString (Tokens s)) => Parser s (Operation RAW)
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

parseOperation :: (Stream s, Term s, IsString s, IsString (Tokens s)) => Parser s (Operation RAW)
parseOperation = parseAnonymousQuery <|> parseOperationDefinition
