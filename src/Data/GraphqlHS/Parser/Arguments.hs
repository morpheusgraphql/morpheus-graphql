module Data.GraphqlHS.Parser.Arguments
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
import           Data.GraphqlHS.Types.Types     ( Arguments
                                                , GQLPrimitive(JSEnum)
                                                , Arg(..)
                                                )
import           Data.GraphqlHS.Parser.Primitive
                                                ( jsString
                                                , token
                                                , variable
                                                )

inputValue :: Parser Arg
inputValue =
    ((ArgValue . JSEnum) <$> token)
        <|> skipSpace
        *>  (ArgValue <$> jsString)
        <|> (Var <$> variable)

parameter :: Parser (Text, Arg)
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
