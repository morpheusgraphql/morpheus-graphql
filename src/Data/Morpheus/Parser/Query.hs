{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Query
    ( query
    )
where


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
import           Data.Morpheus.Types.Error     ( GQLError )
import           Data.Data                      ( Data )
import           Data.Morpheus.Parser.Primitive ( token)
import      Data.Morpheus.Parser.RootHead      (rootHeadArguments)
import           Data.Text                      (Text)
import           Data.Morpheus.Types.Types     ( QuerySelection(..)
                                                , SelectionSet
                                                , Arguments(..)
                                                , GQLOperator(..)
                                                )
import           Data.Morpheus.Parser.Body     ( body )

queryHead :: Parser  (Text,Arguments)
queryHead = do
    string "query "
    skipSpace
    queryName <- token
    variables <- try (skipSpace *> rootHeadArguments) <|> pure []
    pure (queryName, variables)

query:: Parser GQLOperator
query = do
  (queryName, args) <- try (skipSpace *> queryHead) <|> pure ("",[])
  selection <- body args
  pure $ QueryOperator queryName selection