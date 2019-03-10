{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import qualified Data.Text                     as T
                                                ( Text
                                                , pack
                                                , concat
                                                )
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text          as AT
                                                ( takeWhile )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Control.Applicative
import           Data.Functor
import           Data.Char                      ( isAlpha
                                                , isDigit
                                                , isSpace
                                                , ord
                                                )

import qualified Data.Attoparsec.Internal.Types
                                               as AT

replaceType :: T.Text -> T.Text
replaceType "type" = "_type"
replaceType x      = x

boolTrue :: Parser JSType
boolTrue = string "true" $> JSBool True

boolFalse :: Parser JSType
boolFalse = string "false" $> JSBool False

jsBool :: Parser JSType
jsBool = boolTrue <|> boolFalse

jsInt :: Parser JSType
jsInt = JSInt <$> decimal

codes :: String
codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']

replacements :: String
replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

escaped :: Parser Char
escaped = do
    x <- notChar '\"'
    if x == '\\' then choice (zipWith escapeChar codes replacements) else pure x
    where escapeChar code replacement = char code >> return replacement

jsString :: Parser JSType
jsString = do
    char '"'
    value <- many escaped
    char '"'
    pure $ JSString $ T.pack value

token :: Parser T.Text
token = replaceType . T.pack <$> some (letter <|> char '_')

variable :: Parser T.Text
variable = skipSpace *> char '$' *> token

separator :: Parser Char
separator = char ',' <|> char ' ' <|> char '\n' <|> char '\t'

getPosition :: Parser Int
getPosition = AT.Parser internFunc
    where internFunc t pos more _ succ = succ t pos more (AT.fromPos pos)

getNextLine :: Parser Int
getNextLine = do
    _     <- many (notChar '\n')
    index <- getPosition
    char '\n'
    pure index

getLines :: Parser [Int]
getLines = many getNextLine