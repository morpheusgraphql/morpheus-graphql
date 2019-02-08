{-# LANGUAGE OverloadedStrings #-}

module Data.GraphqlHS.Parser.Fragment
(fragment)
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
import           Data.GraphqlHS.Types.Types     ( Fragment(..) )
import           Data.GraphqlHS.ErrorMessage    ( syntaxError
                                                , semanticError
                                                )
import           Data.GraphqlHS.Parser.Primitive (token)
import           Data.GraphqlHS.Parser.Body      (body)

fragment :: Parser (Text,Fragment)
fragment = do
    skipSpace
    string "fragment"
    skipSpace
    name <- token
    skipSpace
    string "on"
    skipSpace
    targetName <- token
    skipSpace
    fragmentBody  <- body
    pure $ (name, Fragment name targetName fragmentBody)

