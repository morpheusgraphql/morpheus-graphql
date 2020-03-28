{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Data.Morpheus.Parsing.Request.Operation
  ( parseOperation
  )
where

import           Data.Functor                   ( ($>) )
import           Text.Megaparsec                ( label
                                                , optional
                                                , (<?>)
                                                , (<|>)
                                                )
import           Text.Megaparsec.Char           ( string )

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , getLocation
                                                )
import           Data.Morpheus.Parsing.Internal.Pattern
                                                ( optionalDirectives )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( operator
                                                , uniqTupleOpt
                                                , parseName
                                                , parseType
                                                , spaceAndComments1
                                                , variable
                                                )
import           Data.Morpheus.Parsing.Internal.Value
                                                ( parseDefaultValue )
import           Data.Morpheus.Parsing.Request.Selection
                                                ( parseSelectionSet )
import           Data.Morpheus.Types.Internal.Operation 
                                                (empty)
import           Data.Morpheus.Types.Internal.AST
                                                ( Operation(..)
                                                , RawOperation
                                                , Variable(..)
                                                , OperationType(..)
                                                , Ref(..)
                                                , VariableContent(..)
                                                , RAW
                                                )


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
  defaultValue <- parseDefaultValue
  pure Variable 
    { variableValue    = DefaultValue defaultValue
    , ..
    }

-- Operations : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Operations
--
-- OperationDefinition
--   OperationType Name(opt) VariableDefinitions(opt) Directives(opt) SelectionSet
--
--   OperationType: one of
--     query, mutation,    subscription
parseOperationDefinition :: Parser RawOperation
parseOperationDefinition = label "OperationDefinition" $ do
  operationPosition  <- getLocation
  operationType      <- parseOperationType
  operationName      <- optional parseName
  operationArguments <- uniqTupleOpt variableDefinition
  -- TODO: handle directives
  _directives        <- optionalDirectives
  operationSelection <- parseSelectionSet
  pure
    (Operation { operationName
               , operationType
               , operationArguments
               , operationSelection
               , operationPosition
               }
    )

parseOperationType :: Parser OperationType
parseOperationType = label "OperationType" $ do
  kind <-
    (string "query" $> Query)
    <|> (string "mutation" $> Mutation)
    <|> (string "subscription" $> Subscription)
  spaceAndComments1
  return kind

parseAnonymousQuery :: Parser RawOperation
parseAnonymousQuery = label "AnonymousQuery" $ do
  operationPosition  <- getLocation
  operationSelection <- parseSelectionSet
  pure
      (Operation { operationName      = Nothing
                 , operationType      = Query
                 , operationArguments = empty
                 , ..
                 }
      )
    <?> "can't parse AnonymousQuery"

parseOperation :: Parser RawOperation
parseOperation = parseAnonymousQuery <|> parseOperationDefinition
