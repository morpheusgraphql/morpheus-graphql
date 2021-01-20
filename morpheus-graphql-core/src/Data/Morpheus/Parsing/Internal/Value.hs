{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Value
  ( enumValue,
    parseDefaultValue,
    Parse (..),
  )
where

import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( Term,
    brackets,
    equal,
    fieldNameColon,
    ignoredTokens,
    number,
    parseNegativeSign,
    parseString,
    parseTypeName,
    setOf,
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
import Relude
import Text.Megaparsec
  ( Stream,
    Tokens,
    label,
    sepBy,
  )
import Text.Megaparsec.Byte
  ( string,
  )

valueNull :: (Stream s, IsString (Tokens s)) => Parser s (Value a)
valueNull = string "null" $> Null

booleanValue :: (Stream s, IsString (Tokens s)) => Parser s (Value a)
booleanValue = boolTrue <|> boolFalse
  where
    boolTrue = string "true" $> Scalar (Boolean True)
    boolFalse = string "false" $> Scalar (Boolean False)

valueNumber :: (Stream s, Term s) => Parser s (Value a)
valueNumber =
  Scalar . decodeScientific
    <$> (signedNumber <$> parseNegativeSign <*> number)
  where
    signedNumber isNegative n
      | isNegative = - n
      | otherwise = n

enumValue :: (Stream s, Term s) => Parser s (Value a)
enumValue = Enum <$> parseTypeName <* ignoredTokens

stringValue :: (Stream s, Term s, IsString s) => Parser s (Value a)
stringValue = label "stringValue" $ Scalar . String <$> parseString

listValue :: (Stream s, Term s) => Parser s a -> Parser s [a]
listValue parser = label "list" $ brackets (parser `sepBy` ignoredTokens)

objectEntry :: (Stream s, Term s) => Parser s (Value a) -> Parser s (ObjectEntry a)
objectEntry parser =
  label "ObjectEntry" $
    ObjectEntry <$> fieldNameColon <*> parser

objectValue :: (Stream s, Term s) => Parser s (Value a) -> Parser s (OrdMap FieldName (ObjectEntry a))
objectValue = label "ObjectValue" . setOf . objectEntry

parsePrimitives :: (Stream s, Term s, IsString s, IsString (Tokens s)) => Parser s (Value a)
parsePrimitives =
  valueNull <|> booleanValue <|> valueNumber <|> enumValue <|> stringValue

parseDefaultValue :: (Stream s, Term s) => Parser s (Value st)
parseDefaultValue = equal *> parseV
  where
    parseV = structValue parseV

class Parse s a where
  parse :: Parser s a

instance (Stream s, Term s) => Parse s (Value RAW) where
  parse = (VariableValue <$> variable) <|> structValue parse

instance (Stream s, Term s) => Parse s (Value CONST) where
  parse = structValue parse

structValue :: (Stream s, Term s) => Parser s (Value a) -> Parser s (Value a)
structValue parser =
  label "Value" $
    ( parsePrimitives
        <|> (Object <$> objectValue parser)
        <|> (List <$> listValue parser)
    )
      <* ignoredTokens
