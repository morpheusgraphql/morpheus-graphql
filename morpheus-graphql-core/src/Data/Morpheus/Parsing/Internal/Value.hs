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
    parseAssignment,
    parseName,
    parseNegativeSign,
    parseTypeName,
    setOf,
    symbol,
    variable,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    ObjectEntry (..),
    OrdMap,
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

objectValue :: Parser (Value a) -> Parser (OrdMap FieldName (ObjectEntry a))
objectValue = label "ObjectValue" . setOf . objectEntry

parsePrimitives :: Parser (Value a)
parsePrimitives =
  valueNull <|> booleanValue <|> valueNumber <|> enumValue <|> stringValue

parseDefaultValue :: Parser ResolvedValue
parseDefaultValue = do
  symbol '='
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
