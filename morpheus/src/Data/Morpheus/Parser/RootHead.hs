module Data.Morpheus.Parser.RootHead (rootHeadArguments) where

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
import           Data.Morpheus.Types.Types     ( Arguments
                                                , Argument(..)
                                                )
import           Data.Morpheus.Parser.Primitive
                                                ( token
                                                , variable
                                                )

rootHeadVariable :: Parser (Text, Argument)
rootHeadVariable = do
    skipSpace
    variableName <- variable
    skipSpace
    char ':'
    skipSpace
    variableType <- token
    pure (variableName, Variable variableType)

rootHeadArguments :: Parser Arguments
rootHeadArguments = do
    skipSpace
    char '('
    skipSpace
    parameters <- rootHeadVariable `sepBy` (skipSpace *> char ',')
    skipSpace
    char ')'
    pure parameters
