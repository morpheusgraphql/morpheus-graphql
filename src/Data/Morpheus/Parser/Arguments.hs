module Data.Morpheus.Parser.Arguments
    ( arguments
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
import           Data.Morpheus.Types.Types     ( Arguments
                                                , JSType(JSEnum)
                                                , Argument(..)
                                                )
import           Data.Morpheus.Parser.Primitive
                                                ( jsString
                                                , token
                                                , variable
                                                )

inputValue :: Parser Argument
inputValue =
    (Argument . JSEnum) <$> token
        <|> skipSpace
        *>  (Argument <$> jsString)
        <|> (Variable <$> variable)

parameter :: Parser (Text, Argument)
parameter = do
    skipSpace
    key <- token
    skipSpace
    char ':'
    skipSpace
    value <- inputValue
    pure (key, value)

arguments :: Parser Arguments
arguments = do
    skipSpace
    char '('
    skipSpace
    parameters <- parameter `sepBy` (skipSpace *> char ',')
    skipSpace
    char ')'
    pure parameters
