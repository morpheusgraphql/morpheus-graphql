{-# LANGUAGE FlexibleInstances #-}
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
  ( brackets,
    comma,
    equal,
    fieldNameColon,
    ignoredTokens,
    parseNegativeSign,
    parseString,
    parseTypeName,
    setOf,
    symbol,
    variable,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    FieldName,
    ObjectEntry (..),
    OrdMap,
    RAW,
    ScalarValue (..),
    Value (..),
    decodeScientific,
  )
import Text.Megaparsec
  ( (<|>),
    label,
    sepBy,
  )
import Text.Megaparsec.Byte
  ( string,
  )
import Text.Megaparsec.Byte.Lexer (scientific)

valueNull :: Parser (Value a)
valueNull = string "null" $> Null

booleanValue :: Parser (Value a)
booleanValue = boolTrue <|> boolFalse
  where
    boolTrue = string "true" $> Scalar (Boolean True)
    boolFalse = string "false" $> Scalar (Boolean False)

valueNumber :: Parser (Value a)
valueNumber =
  Scalar . decodeScientific
    <$> (signedNumber <$> parseNegativeSign <*> scientific)
  where
    signedNumber isNegative number
      | isNegative = - number
      | otherwise = number

enumValue :: Parser (Value a)
enumValue = Enum <$> parseTypeName <* ignoredTokens

stringValue :: Parser (Value a)
stringValue = label "stringValue" $ Scalar . String <$> parseString

listValue :: Parser a -> Parser [a]
listValue parser = label "list" $ brackets (parser `sepBy` ignoredTokens)

objectEntry :: Parser (Value a) -> Parser (ObjectEntry a)
objectEntry parser =
  label "ObjectEntry" $
    ObjectEntry <$> fieldNameColon <*> parser

objectValue :: Parser (Value a) -> Parser (OrdMap FieldName (ObjectEntry a))
objectValue = label "ObjectValue" . setOf . objectEntry

parsePrimitives :: Parser (Value a)
parsePrimitives =
  valueNull <|> booleanValue <|> valueNumber <|> enumValue <|> stringValue

parseDefaultValue :: Parser (Value s)
parseDefaultValue = equal *> parseV
  where
    parseV :: Parser (Value s)
    parseV = structValue parseV

class Parse a where
  parse :: Parser a

instance Parse (Value RAW) where
  parse = (VariableValue <$> variable) <|> structValue parse

instance Parse (Value CONST) where
  parse = structValue parse

structValue :: Parser (Value a) -> Parser (Value a)
structValue parser =
  label "Value" $
    ( parsePrimitives
        <|> (Object <$> objectValue parser)
        <|> (List <$> listValue parser)
    )
      <* ignoredTokens
