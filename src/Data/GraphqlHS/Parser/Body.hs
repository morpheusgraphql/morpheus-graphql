{-# LANGUAGE OverloadedStrings #-}

module Data.GraphqlHS.Parser.Body (body)
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
import           Data.GraphqlHS.Types.Types     ( GQLValue(..)
                                                , Object
                                                ,Head(..)
                                                )
import           Data.GraphqlHS.Parser.Arguments   ( arguments )
import           Data.GraphqlHS.Parser.Primitive   ( token  , field , seperator )

spread :: Parser (Text, GQLValue)
spread = do
    string "..."
    key <- some (letter <|> char '_')
    return (pack key, Spread $ pack key)

entry :: Parser (Text, GQLValue)
entry = do
    skipSpace
    key        <- token
    args <- (try arguments) <|> (pure Empty)
    value      <- (try body) <|> (field key)
    case args of
        Empty -> return (key, value)
        x     -> return (key, Query x value)

body :: Parser GQLValue
body =
   skipSpace
        *> char '{'
        *> skipSpace
        *> ( (Object . fromList) <$> (( entry <|> spread ) `sepBy` seperator))
        <* skipSpace
        <* char '}'