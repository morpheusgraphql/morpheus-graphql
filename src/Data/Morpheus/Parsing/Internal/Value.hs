{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Parsing.Internal.Value
  ( parseValue
  , enumValue
  , parseDefaultValue
  ) where

import           Data.Functor                            (($>))
import           Data.Text                               (pack)
import           Text.Megaparsec                         (anySingleBut, between, choice, label, many, optional, sepBy,
                                                          (<|>))
import           Text.Megaparsec.Char                    (char, string)
import           Text.Megaparsec.Char.Lexer              (scientific)

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (litEquals, parseAssignment, setOf, spaceAndComments, token)
import           Data.Morpheus.Types.Internal.Value      (ScalarValue (..), Value (..), decodeScientific)

parseDefaultValue :: Parser (Maybe Value)
parseDefaultValue =
  optional $ do
    litEquals
    parseValue

parseValue :: Parser Value
parseValue =
  label "value" $ do
    value <- valueNull <|> booleanValue <|> valueNumber <|> enumValue <|> stringValue <|> objectValue <|> listValue
    spaceAndComments
    return value

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
enumValue = do
  enum <- Enum <$> token
  spaceAndComments
  return enum

escaped :: Parser Char
escaped =
  label "escaped" $ do
    x <- anySingleBut '\"'
    if x == '\\'
      then choice (zipWith escapeChar codes replacements)
      else pure x
  where
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']
    codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
    escapeChar code replacement = char code >> return replacement

stringValue :: Parser Value
stringValue = label "stringValue" $ Scalar . String . pack <$> between (char '"') (char '"') (many escaped)

listValue :: Parser Value
listValue =
  label "listValue" $
  List <$>
  between
    (char '[' *> spaceAndComments)
    (char ']' *> spaceAndComments)
    (parseValue `sepBy` (char ',' *> spaceAndComments))

objectValue :: Parser Value
objectValue = label "objectValue" $ Object <$> setOf entry
  where
    entry = parseAssignment token parseValue
