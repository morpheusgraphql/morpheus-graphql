{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Parsing.Internal.Value
  ( parseValue
  , enumValue
  , parseDefaultValue
  , parseRawValue
  )
where

import           Data.Functor                   ( ($>) )
import           Data.Text                      ( pack )
import           Text.Megaparsec                ( anySingleBut
                                                , between
                                                , choice
                                                , label
                                                , many
                                                , optional
                                                , sepBy
                                                , (<|>)
                                                )
import           Text.Megaparsec.Char           ( char
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( scientific )

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( litEquals
                                                , parseAssignment
                                                , setOf
                                                , spaceAndComments
                                                , token
                                                , parseNegativeSign
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( ScalarValue(..)
                                                , Value(..)
                                                , RawValue
                                                , ValidValue
                                                , decodeScientific
                                                , Name
                                                , Value(..)
                                                )

valueNull :: Parser (Value a)
valueNull = string "null" $> Null

booleanValue :: Parser (Value a)
booleanValue = boolTrue <|> boolFalse
 where
  boolTrue  = string "true" $> Scalar (Boolean True)
  boolFalse = string "false" $> Scalar (Boolean False)

valueNumber :: Parser (Value a)
valueNumber = do
  isNegative <- parseNegativeSign
  Scalar . decodeScientific . signedNumber isNegative <$> scientific
 where
  signedNumber isNegative number | isNegative = -number
                                 | otherwise  = number

enumValue :: Parser (Value a)
enumValue = do
  enum <- Enum <$> token
  spaceAndComments
  return enum

escaped :: Parser Char
escaped = label "escaped" $ do
  x <- anySingleBut '\"'
  if x == '\\' then choice (zipWith escapeChar codes replacements) else pure x
 where
  replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']
  codes        = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
  escapeChar code replacement = char code >> return replacement

stringValue :: Parser (Value a)
stringValue = label "stringValue" $ Scalar . String . pack <$> between
  (char '"')
  (char '"')
  (many escaped)


listValue :: Parser a -> Parser [a]
listValue parser = label "listValue" $ between
  (char '[' *> spaceAndComments)
  (char ']' *> spaceAndComments)
  (parser `sepBy` (char ',' *> spaceAndComments))

objectValue :: Show a => Parser a -> Parser [(Name, a)]
objectValue parser = label "objectValue" $ setOf entry
  where entry = parseAssignment token parser

parsePrimitives :: Parser (Value a)
parsePrimitives =
  valueNull <|> booleanValue <|> valueNumber <|> enumValue <|> stringValue


parseDefaultValue :: Parser (Maybe ValidValue)
parseDefaultValue = optional $ do
  litEquals
  parseValue

parseGenValue :: Parser (Value a)
parseGenValue = label "value" $ do
  value <-
    parsePrimitives
    <|> (Object <$> objectValue parseGenValue)
    <|> (List <$> listValue parseGenValue)
  spaceAndComments
  return value

parseValue :: Parser ValidValue
parseValue = parseGenValue

parseRawValue :: Parser RawValue
parseRawValue = parseGenValue
