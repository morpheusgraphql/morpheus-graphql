module Data.Morpheus.Parser.InputValues.InputObject
  ( inputObject
  ) where

import           Control.Applicative            ((<|>))
import           Data.Attoparsec.Text           (Parser, char, sepBy, skipSpace, try)
import           Data.Morpheus.Parser.Primitive (jsString, token)
import           Data.Morpheus.Types.JSType     (JSType (..))
import           Data.Text                      (Text)

entry :: Parser (Text, JSType)
entry = do
  skipSpace --index <- getPosition
  key <- token
  skipSpace
  _ <- char ':'
  skipSpace
  value <- try inputObject <|> jsString
  return (key, value)

entries :: Parser [(Text, JSType)]
entries = do
  _ <- char '{'
  skipSpace
  entries' <- entry `sepBy` char ','
  skipSpace
  _ <- char '}'
  return entries'

inputObject :: Parser JSType
inputObject = do
  skipSpace -- index <- getPosition
  JSObject <$> entries
