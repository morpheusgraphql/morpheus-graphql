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
                                                , getPosition
                                                )


enum = JSEnum <$> token

argumentType :: Parser Argument
argumentType = do
    pos <- getPosition
    arg <- enum <|> jsString <|> jsBool <|> jsInt
    pure $ Argument arg pos

variableType :: Parser Argument
variableType = do
    pos <- getPosition
    val <- variable
    pure $ Variable val pos

inputValue :: Parser Argument
inputValue = skipSpace *> argumentType <|> variableType

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
