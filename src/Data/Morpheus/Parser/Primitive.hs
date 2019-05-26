{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import           Control.Applicative                (many, (<|>))
import           Data.Attoparsec.Text
import           Data.Functor                       (($>))
import           Data.Morpheus.Parser.Internal      (getPosition)
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..), Value (..), decodeScientific)
import           Data.Text                          (Text)
import qualified Data.Text                          as T (pack)

replaceType :: Text -> Text
replaceType "type" = "_type"
replaceType x      = x

boolTrue :: Parser Value
boolTrue = string "true" $> Scalar (Boolean True)

valueNull :: Parser Value
valueNull = string "null" $> Null

boolFalse :: Parser Value
boolFalse = string "false" $> Scalar (Boolean False)

valueBoolean :: Parser Value
valueBoolean = boolTrue <|> boolFalse

valueNumber :: Parser Value
valueNumber = Scalar . decodeScientific <$> scientific

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

valueString :: Parser Value
valueString = do
  _ <- char '"'
  value <- many escaped
  _ <- char '"'
  pure $ Scalar $ String $ T.pack value

token :: Parser Text
token = do
  firstChar <- letter <|> char '_'
  restToken <- many $ letter <|> char '_' <|> digit
  return $ replaceType $ T.pack $ firstChar : restToken

qualifier :: Parser (Text, Int)
qualifier = do
  skipSpace
  position' <- getPosition
  value <- token
  return (value, position')

variable :: Parser (Text, Int)
variable = do
  skipSpace
  position' <- getPosition
  _ <- char '$'
  varName' <- token
  return (varName', position')

separator :: Parser Char
separator = char ',' <|> char ' ' <|> char '\n' <|> char '\t'

getNextLine :: Parser Int
getNextLine = do
  _ <- many (notChar '\n')
  index <- getPosition
  _ <- char '\n'
  pure index

getLines :: Parser [Int]
getLines = many getNextLine
