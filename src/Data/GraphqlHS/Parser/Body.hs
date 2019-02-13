{-# LANGUAGE OverloadedStrings #-}

module Data.GraphqlHS.Parser.Body
    ( body
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
import           Data.GraphqlHS.Types.Types     ( QuerySelection(..)
                                                , SelectionSet
                                                , Arguments(..)
                                                )
import           Data.GraphqlHS.Parser.Arguments
                                                ( arguments )
import           Data.GraphqlHS.Parser.Primitive
                                                ( token
                                                , seperator
                                                )

spread :: Parser (Text, QuerySelection)
spread = do
    string "..."
    key <- some (letter <|> char '_')
    return (pack key, Spread $ pack key)

entry :: Parser (Text, QuerySelection)
entry = do
    skipSpace
    key   <- token
    args  <- (try arguments) <|> (pure [])
    value <- (try $ body args) <|> (pure $ Field args key)
    return (key, value)

body :: Arguments -> Parser QuerySelection
body args =
    skipSpace
        *> char '{'
        *> skipSpace
        *> (   (SelectionSet args . fromList)
           <$> ((entry <|> spread) `sepBy` seperator)
           )
        <* skipSpace
        <* char '}'
