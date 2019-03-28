{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Functor
import           Data.Morpheus.Types.JSType     (JSType (..), Scalar (..))
import qualified Data.Text                      as T (Text, pack)

import qualified Data.Attoparsec.Internal.Types as AT

replaceType :: T.Text -> T.Text
replaceType "type" = "_type"
replaceType x      = x

boolTrue :: Parser JSType
boolTrue = string "true" $> Scalar (Boolean True)

boolFalse :: Parser JSType
boolFalse = string "false" $> Scalar (Boolean False)

jsBool :: Parser JSType
jsBool = boolTrue <|> boolFalse

jsInt :: Parser JSType
jsInt = Scalar . Int <$> decimal

codes :: String
codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']

replacements :: String
replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

escaped :: Parser Char
escaped = do
  x <- notChar '\"'
  if x == '\\'
    then choice (zipWith escapeChar codes replacements)
    else pure x
  where
    escapeChar code replacement = char code >> return replacement

jsString :: Parser JSType
jsString = do
  _ <- char '"'
  value <- many escaped
  _ <- char '"'
  pure $ Scalar $ String $ T.pack value

token :: Parser T.Text
token = replaceType . T.pack <$> some (letter <|> char '_')

variable :: Parser T.Text
variable = skipSpace *> char '$' *> token

separator :: Parser Char
separator = char ',' <|> char ' ' <|> char '\n' <|> char '\t'

getPosition :: Parser Int
getPosition = AT.Parser internFunc
  where
    internFunc t pos more _ success = success t pos more (AT.fromPos pos)

getNextLine :: Parser Int
getNextLine = do
  _ <- many (notChar '\n')
  index <- getPosition
  _ <- char '\n'
  pure index

getLines :: Parser [Int]
getLines = many getNextLine
