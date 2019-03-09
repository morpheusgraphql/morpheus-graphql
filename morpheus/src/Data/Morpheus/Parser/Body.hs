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
                                                , ParserPosition(..)
                                                )
import           Data.Morpheus.Parser.Arguments ( arguments )
import           Data.Morpheus.Parser.Primitive ( token
                                                , separator
                                                )

import qualified Data.Attoparsec.Internal.Types
                                               as AT

getPosition :: Parser ParserPosition
getPosition = ParserPosition <$> (AT.Parser internFunc)
    where internFunc t pos more _ succ' = succ' t pos more (AT.fromPos pos)

spread :: Parser (Text, QuerySelection)
spread = do
    skipSpace
    string "..."
    key      <- some (letter <|> char '_')
    position <- getPosition
    return (pack key, Spread (pack key) position)

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
