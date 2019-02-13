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
import           Data.GraphqlHS.Types.Types     ( Eval
                                                , GQLQueryRoot(..)
                                                , GQLRequest(..)
                                                )
import           Data.GraphqlHS.Types.Error     ( GQLError )
import           Data.GraphqlHS.ErrorMessage    ( syntaxError )
import qualified Data.GraphqlHS.Parser.Query   as B
import           Data.GraphqlHS.Parser.Body     ( body )
import           Data.GraphqlHS.Parser.Fragment ( fragment )


request :: Parser GQLQueryRoot
request = do
    queryName      <- (try (skipSpace *> B.query)) <|> pure ""
    queryBodyValue <- body []
    fragmentLib    <- fromList <$> (many fragment)
    skipSpace
    endOfInput
    pure GQLQueryRoot
        { queryBody      = queryBodyValue
        , fragments      = fragmentLib
        , inputVariables = fromList []
        }

getVariables req = case variables req of
    Nothing   -> fromList []
    Just vars -> vars

parseGQL :: GQLRequest -> Eval GQLQueryRoot
parseGQL requestBody = case (parseOnly request $ query requestBody) of
    Right root  -> Right (root { inputVariables = getVariables requestBody })
    Left  error -> Left $ syntaxError $ pack $ show error
