{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Parser
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
import           Data.Morpheus.Types.Types      ( Validation
                                                , GQLQueryRoot(..)
                                                , GQLRequest(..)
                                                )
import           Data.Morpheus.Types.Error      ( GQLError )
import           Data.Morpheus.ErrorMessage     ( syntaxError )
import qualified Data.Morpheus.Parser.Query    as Q
import           Data.Morpheus.Parser.Body      ( body )
import           Data.Morpheus.Parser.Fragment  ( fragment )
import qualified Data.Morpheus.Parser.Mutation as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Morpheus.Parser.Primitive ( getLines )
import           Data.Morpheus.Types.MetaInfo   (Position(..))


request :: Parser GQLQueryRoot
request = do
    queryValue  <- Q.query <|> M.mutation
    fragmentLib <- fromList <$> many fragment
    skipSpace
    endOfInput
    pure GQLQueryRoot
        { lineMarks      = []
        , queryBody      = queryValue
        , fragments      = fragmentLib
        , inputVariables = fromList []
        }

getVariables = fromMaybe (fromList []) . variables

parseReq requestBody = parseOnly request $ query requestBody

scanLines requestBody root = do 
    lines <- parseOnly getLines $ query requestBody 
    pure root { lineMarks = lines}


parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL requestBody = case parseReq requestBody >>= scanLines requestBody of
    Right root  -> Right $ root { inputVariables = getVariables requestBody }
    Left  error -> Left $ syntaxError (pack $ show error) [] (Position 0)
