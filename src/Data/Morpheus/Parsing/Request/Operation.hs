{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Operation
  ( parseAnonymousQuery
  , parseOperation
  ) where

import           Data.Functor                               (($>))
import           Data.Text                                  (Text)
import           Text.Megaparsec                            (label, optional, (<?>), (<|>))
import           Text.Megaparsec.Char                       (string)

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal    (Parser, getLocation)
import           Data.Morpheus.Parsing.Internal.Terms       (operator, parseMaybeTuple, parseType, spaceAndComments1,
                                                             token, variable)
import           Data.Morpheus.Parsing.Internal.Value       (parseDefaultValue)
import           Data.Morpheus.Parsing.Request.Body         (entries)
import           Data.Morpheus.Types.Internal.AST.Operation (DefaultValue, Operation (..), RawOperation, Variable (..))
import           Data.Morpheus.Types.Internal.Data          (OperationKind (..), isNullable)


-- Variables :  https://graphql.github.io/graphql-spec/June2018/#VariableDefinition
--
--  VariableDefinition
--    Variable : Type DefaultValue(opt)
--
variableDefinition :: Parser (Text, Variable DefaultValue)
variableDefinition =
  label "VariableDefinition" $ do
    (name, variablePosition) <- variable
    operator ':'
    (variableTypeWrappers, variableType) <- parseType
    defaultValue <- parseDefaultValue
    pure
      ( name
      , Variable
          { variableType
          , isVariableRequired = not (isNullable variableTypeWrappers)
          , variableTypeWrappers
          , variablePosition
          , variableValue = defaultValue
          })

-- Operations : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Operations
--
-- OperationDefinition
--   OperationType Name(opt) VariableDefinitions(opt) Directives(opt) SelectionSet
--
--   OperationType: one of
--     query, mutation,    subscription
parseOperation :: Parser RawOperation
parseOperation =
  label "operator" $ do
    operationPosition <- getLocation
    operationKind <- parseOperationType
    operationName <- optional token
    operationArgs <- parseMaybeTuple variableDefinition
    operationSelection <- entries
    pure (Operation {operationName, operationKind, operationArgs, operationSelection, operationPosition})


parseOperationType :: Parser OperationKind
parseOperationType =
  label "OperationType" $ do
    kind <- (string "query" $> Query) <|> (string "mutation" $> Mutation) <|> (string "subscription" $> Subscription)
    spaceAndComments1
    return kind

parseAnonymousQuery :: Parser RawOperation
parseAnonymousQuery =
  label "AnonymousQuery" $ do
    operationPosition <- getLocation
    operationSelection <- entries
    pure
      (Operation
         { operationName = Nothing
         , operationKind = Query
         , operationArgs = []
         , operationSelection
         , operationPosition
         }) <?>
      "can't parse AnonymousQuery"


