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
import           Control.Applicative
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Control.Applicative
import           Data.Char
import           Data.Char                      ( isAlpha
                                                , isDigit
                                                , isSpace
                                                , ord
                                                )

import           Data.Morpheus.Types.Error      ( ErrorLocation(..) )
import           Data.List                      ( filter )

replaceType :: T.Text -> T.Text
replaceType "type" = "_type"
replaceType x      = x

boolTrue :: Parser JSType
boolTrue = string "true" *> pure (JSBool True)

boolFalse :: Parser JSType
boolFalse = string "false" *> pure (JSBool False)

jsBool :: Parser JSType
jsBool = boolTrue <|> boolFalse

jsInt :: Parser JSType
jsInt = JSInt <$> decimal

codes :: [Char]
codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']

replacements :: [Char]
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

type WithPos a = (Int, Int, a)

type Pos = ErrorLocation

data Spaces = Line | Tab | Column deriving  ( Eq , Show) ;

skipRow :: Parser Spaces
skipRow = char '\n' >> pure Line

skipTab :: Parser Spaces
skipTab = char '\t' >> pure Tab

skipEmpty :: Parser Spaces
skipEmpty = char ' ' >> pure Column

skipColumnRowSingle :: Parser Spaces
skipColumnRowSingle = skipRow <|> skipTab <|> skipEmpty


countLines = length . filter (== Line) 

countColumns = length . filter (==Column)

countMany :: Pos -> [Spaces] -> Pos
countMany pos list = pos { line   = countLines list + (line pos)
                         , column = countColumns list + (column pos)
                         }

skipColumnRow :: Pos -> Parser Pos
skipColumnRow pos = countMany pos <$> (many skipColumnRowSingle)
