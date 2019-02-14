{-# LANGUAGE OverloadedStrings #-}

module Data.GraphqlHS.Parser.Query
    ( query
    )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.Map                       ( fromList )
import           Data.Attoparsec.Text           ( Parser
                                                , char
                                                , letter
                                                , sepBy
                                                , skipSpace
                                                , try
                                                , parseOnly
                                                , parse
                                                , IResult(Done)
                                                , string
                                                , endOfInput
                                                )
import           Control.Applicative            ( (<|>)
                                                , many
                                                , some
                                                )
import           Data.GraphqlHS.Types.Types     ( Arguments
                                                , Argument(..)
                                                )

import           Data.GraphqlHS.Parser.Arguments
                                                ( arguments )
import           Data.GraphqlHS.Types.Error     ( GQLError )
import           Data.Data                      ( Data )
import           Data.GraphqlHS.ErrorMessage    ( syntaxError
                                                , semanticError
                                                )
import           Data.GraphqlHS.Parser.Primitive
                                                ( token
                                                , variable
                                                )

queryVariable :: Parser (Text, Argument)
queryVariable = do
    skipSpace
    variableName <- variable
    skipSpace
    char ':'
    skipSpace
    variableType <- token
    pure (variableName, Variable variableType)

queryArguments :: Parser (Maybe Arguments)
queryArguments = do
    skipSpace
    char '('
    skipSpace
    parameters <- queryVariable `sepBy` (skipSpace *> char ',')
    skipSpace
    char ')'
    pure $ Just parameters

query :: Parser Text
query = do
    string "query "
    skipSpace
    queryName <- token
    variables <- (try (skipSpace *> queryArguments)) <|> (pure Nothing)
    pure queryName


