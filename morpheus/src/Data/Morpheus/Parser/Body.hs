{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
    ( body
    )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
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
import           Data.Morpheus.Types.Types      ( QuerySelection(..)
                                                , SelectionSet
                                                , Arguments(..)
                                                )
import           Data.Morpheus.Parser.Arguments ( arguments )
import           Data.Morpheus.Parser.Primitive ( token
                                                , separator
                                                )

spread :: Parser (Text, QuerySelection)
spread = do
    skipSpace
    string "..."
    key <- some (letter <|> char '_')
    return (pack key, Spread $ pack key)

entry :: Parser (Text, QuerySelection)
entry = do
    skipSpace
    key   <- token
    args  <- try arguments <|> pure []
    value <- (try $ body args) <|> (pure $ Field args key)
    return (key, value)

body :: Arguments -> Parser QuerySelection
body args =
    skipSpace
        *> char '{'
        *> skipSpace
        *> (SelectionSet args <$> ((entry <|> spread) `sepBy` separator))
        <* skipSpace
        <* char '}'
