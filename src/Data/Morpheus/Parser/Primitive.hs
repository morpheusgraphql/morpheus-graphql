{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import           Control.Applicative                (many, (<|>))
import           Data.Attoparsec.Text
import           Data.Functor                       (($>))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..), Value (..), decodeScientific)
import qualified Data.Text                          as T (Text, pack)

import qualified Data.Attoparsec.Internal.Types     as AT

replaceType :: T.Text -> T.Text
replaceType "type" = "_type"
replaceType x      = x

boolTrue :: Parser Value
boolTrue = string "true" $> Scalar (Boolean True)

boolFalse :: Parser Value
boolFalse = string "false" $> Scalar (Boolean False)

jsBool :: Parser Value
jsBool = boolTrue <|> boolFalse

jsNumber :: Parser Value
jsNumber = Scalar . decodeScientific <$> scientific

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

jsString :: Parser Value
jsString = do
  _ <- char '"'
  value <- many escaped
  _ <- char '"'
  pure $ Scalar $ String $ T.pack value

token :: Parser T.Text
token = do
  firstChar <- letter <|> char '_'
  restToken <- many $ letter <|> char '_' <|> digit
  return $ replaceType $ T.pack $ firstChar : restToken

variable :: Parser (T.Text, Int)
variable = do
  skipSpace
  position' <- getPosition
  _ <- char '$'
  varName' <- token
  return (varName', position')

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
