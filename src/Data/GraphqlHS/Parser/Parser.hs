{-# LANGUAGE OverloadedStrings #-}

module Data.GraphqlHS.Parser.Parser
    ( parseGQL
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
import           Data.GraphqlHS.Types.Types     ( 
                                                  Eval(..)
                                                , GQLQueryRoot(..)
                                                )
import           Data.GraphqlHS.Types.Error     (GQLError)
import           Data.GraphqlHS.ErrorMessage    (syntaxError)
import           Data.GraphqlHS.Parser.Query    ( query )
import           Data.GraphqlHS.Parser.Body     ( body )
import           Data.GraphqlHS.Parser.Fragment ( fragment )


request :: Parser GQLQueryRoot
request = do
    queryName <- (try (skipSpace *> query)) <|> pure ""
    queryBodyValue     <- body
    fragmentLib <- fromList <$> (many fragment)
    skipSpace
    endOfInput
    pure GQLQueryRoot {
        queryBody = queryBodyValue,
        fragments = fragmentLib
    }

parseGQL :: Text -> Eval GQLQueryRoot
parseGQL x = case (parseOnly request x) of
    Right v     -> Val v
    Left  error -> Fail $ syntaxError $ pack $ show error