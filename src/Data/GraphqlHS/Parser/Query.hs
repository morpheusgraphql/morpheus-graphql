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
import           Data.GraphqlHS.Types.Types     ( Head(..)
                                                , Arg(..)
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

queryVariable :: Parser (Text, Arg)
queryVariable = do
    skipSpace
    variableName <- variable
    skipSpace
    char ':'
    skipSpace
    variableType <- token
    pure (variableName, Var variableType)

queryHead :: Parser (Maybe Head)
queryHead = do
    skipSpace
    char '('
    skipSpace
    parameters <- (fromList <$> (queryVariable `sepBy` (skipSpace *> char ',')))
    skipSpace
    char ')'
    pure $ Just (Arguments parameters)

query :: Parser Text
query = do
    string "query "
    skipSpace
    queryName <- token
    variables <- (try (skipSpace *> queryHead)) <|> (pure Nothing)
    pure queryName


