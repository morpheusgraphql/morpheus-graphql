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
                                                , skipColumnRow
                                                )
import           Data.Morpheus.Types.Error      ( ErrorLocation(..) )

import qualified Data.Attoparsec.Internal.Types
                                               as AT

--getPosition :: Parser ParserPosition
--getPosition = ParserPosition <$> (AT.Parser internFunc)
--    where internFunc t pos more _ succ' = succ' t pos more (AT.fromPos pos)

spread :: Parser (Text, QuerySelection)
spread = do
    position <- skipColumnRow $ ErrorLocation 0 0
    string "..."
    key <- some (letter <|> char '_')
    return (pack key, Spread (pack key) position)

entry :: Parser (Text, QuerySelection)
entry = do
    skipSpace
    key   <- token
    args  <- try arguments <|> pure []
    value <- (try $ body args) <|> (pure $ Field args key)
    return (key, value)

seperated x = x `sepBy` separator

body :: Arguments -> Parser QuerySelection
body args = do
    skipSpace
    char '{'
    position <- skipColumnRow $ ErrorLocation 0 0
    entries  <- seperated $ entry <|> spread
    skipSpace
    char '}'
    return (SelectionSet args entries)
