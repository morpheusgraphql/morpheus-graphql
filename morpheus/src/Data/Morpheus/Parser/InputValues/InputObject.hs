module Data.Morpheus.Parser.InputValues.InputObject
  ( inputObject
  ) where

import           Data.Attoparsec.Text           (Parser, char, sepBy, skipSpace, try)
import           Data.Morpheus.Parser.Primitive (token)
import           Data.Morpheus.Types.JSType     (JSType (..))
import           Data.Text                      (Text)

entry :: Parser JSType -> Parser (Text, JSType)
entry parser = do
  skipSpace
  key <- token
  skipSpace
  _ <- char ':'
  skipSpace
  value <- try parser
  return (key, value)

entries :: Parser JSType -> Parser [(Text, JSType)]
entries parser = do
  _ <- char '{'
  skipSpace
  entries' <- entry parser `sepBy` char ','
  skipSpace
  _ <- char '}'
  return entries'

inputObject :: Parser JSType -> Parser JSType
inputObject parser = do
  skipSpace
  JSObject <$> entries parser
