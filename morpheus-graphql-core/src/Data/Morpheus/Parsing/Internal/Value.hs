{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Parsing.Internal.Value
  ( enumValue,
    parseDefaultValue,
    Parse (..),
  )
where

import Data.Functor (($>))
--
-- MORPHEUS
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( ignoredTokens,
    litEquals,
    parseAssignment,
    parseName,
    parseNegativeSign,
    parseTypeName,
    setOf,
    variable,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    ObjectEntry (..),
    OrderedMap,
    RAW,
    ResolvedValue,
    ScalarValue (..),
    VALID,
    Value (..),
    decodeScientific,
  )
import Data.Text (pack)
import Text.Megaparsec
  ( (<|>),
    anySingleBut,
    between,
    choice,
    label,
    many,
    sepBy,
  )
import Text.Megaparsec.Char
  ( char,
    string,
  )
import Text.Megaparsec.Char.Lexer (scientific)

valueNull :: Parser (Value a)
valueNull = string "null" $> Null

booleanValue :: Parser (Value a)
booleanValue = boolTrue <|> boolFalse
  where
    boolTrue = string "true" $> Scalar (Boolean True)
    boolFalse = string "false" $> Scalar (Boolean False)

valueNumber :: Parser (Value a)
valueNumber = do
  isNegative <- parseNegativeSign
  Scalar . decodeScientific . signedNumber isNegative <$> scientific
  where
    signedNumber isNegative number
      | isNegative = - number
      | otherwise = number

enumValue :: Parser (Value a)
enumValue = do
  enum <- Enum <$> parseTypeName
  ignoredTokens
  return enum

escaped :: Parser Char
escaped = label "escaped" $ do
  x <- anySingleBut '\"'
  if x == '\\' then choice (zipWith escapeChar codes replacements) else pure x
  where
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']
    codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
    escapeChar code replacement = char code >> return replacement

stringValue :: Parser (Value a)
stringValue =
  label "stringValue" $
    Scalar . String . pack
      <$> between
        (char '"')
        (char '"')
        (many escaped)

listValue :: Parser a -> Parser [a]
listValue parser =
  label "ListValue" $
    between
      (char '[' *> ignoredTokens)
      (char ']' *> ignoredTokens)
      (parser `sepBy` (many (char ',') *> ignoredTokens))

objectEntry :: Parser (Value a) -> Parser (ObjectEntry a)
objectEntry parser = label "ObjectEntry" $ do
  (entryName, entryValue) <- parseAssignment parseName parser
  pure ObjectEntry {entryName, entryValue}

objectValue :: Parser (Value a) -> Parser (OrderedMap FieldName (ObjectEntry a))
objectValue = label "ObjectValue" . setOf . objectEntry

parsePrimitives :: Parser (Value a)
parsePrimitives =
  valueNull <|> booleanValue <|> valueNumber <|> enumValue <|> stringValue

parseDefaultValue :: Parser ResolvedValue
parseDefaultValue = do
  litEquals
  parseV
  where
    parseV :: Parser ResolvedValue
    parseV = structValue parseV

class Parse a where
  parse :: Parser a

instance Parse (Value RAW) where
  parse = (VariableValue <$> variable) <|> structValue parse

instance Parse (Value VALID) where
  parse = structValue parse

structValue :: Parser (Value a) -> Parser (Value a)
structValue parser =
  label "Value" $
    ( parsePrimitives
        <|> (Object <$> objectValue parser)
        <|> (List <$> listValue parser)
    )
      <* ignoredTokens
