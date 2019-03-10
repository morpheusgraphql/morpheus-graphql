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
import           Data.Morpheus.Types.Types      ( Arguments
                                                , Argument(..)
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(JSEnum) )
import           Data.Morpheus.Parser.Primitive ( jsString
                                                , token
                                                , variable
                                                , jsBool
                                                , jsInt
                                                )


enum = JSEnum <$> token

argumetType = do
    arg <- enum <|> jsString <|> jsBool <|> jsInt
    pure $ Argument arg 0

variableType = do
    val <- variable
    pure $ Variable val 0

inputValue :: Parser Argument
inputValue = skipSpace *> argumetType <|> variableType

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
