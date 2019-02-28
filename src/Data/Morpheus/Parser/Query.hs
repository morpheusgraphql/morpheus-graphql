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
import           Data.Morpheus.Types.Types     ( Arguments)
import           Data.Morpheus.Types.Error     ( GQLError )
import           Data.Data                      ( Data )
import           Data.Morpheus.Parser.Primitive ( token)
import      Data.Morpheus.Parser.RootHead      (rootHeadArguments)
import           Data.Text                      (Text)

query :: Parser  (Text,Arguments)
query = do
    string "query "
    skipSpace
    queryName <- token
    variables <- try (skipSpace *> rootHeadArguments) <|> pure []
    pure (queryName, variables)


