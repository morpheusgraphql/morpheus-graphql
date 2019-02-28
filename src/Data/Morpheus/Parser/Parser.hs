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
import           Data.Morpheus.Types.Types     ( Validation
                                                , GQLQueryRoot(..)
                                                , GQLRequest(..)
                                                )
import           Data.Morpheus.Types.Error     ( GQLError )
import           Data.Morpheus.ErrorMessage    ( syntaxError )
import qualified Data.Morpheus.Parser.Query   as Q
import           Data.Morpheus.Parser.Body     ( body )
import           Data.Morpheus.Parser.Fragment ( fragment )
import qualified Data.Morpheus.Parser.Mutation as M

request :: Parser GQLQueryRoot
request = do
    queryValue <- Q.query <|> M.mutation
    fragmentLib    <- fromList <$> many fragment
    skipSpace
    endOfInput
    pure GQLQueryRoot
        { queryBody      = queryValue
        , fragments      = fragmentLib
        , inputVariables = fromList []
        }

getVariables req = case variables req of
    Nothing   -> fromList []
    Just vars -> vars

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL requestBody = case parseOnly request $ query requestBody of
    Right root  -> Right $ root { inputVariables = getVariables requestBody }
    Left  error -> Left $ syntaxError $ pack $ show error
