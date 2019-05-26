{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Value
  ( parseValue
  , enumValue
  ) where

import           Control.Applicative                (many, (<|>))
import           Data.Attoparsec.Text               (Parser, char, choice, notChar, scientific, sepBy, skipSpace,
                                                     string)
import           Data.Functor                       (($>))
import           Data.Morpheus.Parser.Primitive     (token)
import           Data.Morpheus.Parser.Terms         (parseAssignment)
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..), Value (..), decodeScientific)
import           Data.Text                          (pack)

parseValue :: Parser Value
parseValue = valueNull <|> booleanValue <|> valueNumber <|> stringValue <|> objectValue <|> listValue

valueNull :: Parser Value
valueNull = string "null" $> Null

booleanValue :: Parser Value
booleanValue = boolTrue <|> boolFalse
  where
    boolTrue = string "true" $> Scalar (Boolean True)
    boolFalse = string "false" $> Scalar (Boolean False)

valueNumber :: Parser Value
valueNumber = Scalar . decodeScientific <$> scientific

enumValue :: Parser Value
enumValue = Enum <$> token

escaped :: Parser Char
escaped = do
  x <- notChar '\"'
  if x == '\\'
    then choice (zipWith escapeChar codes replacements)
    else pure x
  where
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']
    codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
    escapeChar code replacement = char code >> return replacement

stringValue :: Parser Value
stringValue = do
  _ <- char '"'
  value <- many escaped
  _ <- char '"'
  pure $ Scalar $ String $ pack value

listValue :: Parser Value
listValue = do
  skipSpace
  _ <- char '['
  skipSpace
  entries' <-
    (do skipSpace
        val <- parseValue
        skipSpace
        return val) `sepBy`
    char ','
  skipSpace
  _ <- char ']'
  return (List entries')

objectValue :: Parser Value
objectValue = do
  skipSpace
  _ <- char '{'
  skipSpace
  entries' <- parseAssignment token parseValue `sepBy` char ','
  skipSpace
  _ <- char '}'
  return (Object entries')
