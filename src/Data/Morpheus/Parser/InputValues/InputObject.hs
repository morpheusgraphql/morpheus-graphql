module Data.Morpheus.Parser.InputValues.InputObject
  ( inputObject
  ) where

import           Data.Attoparsec.Text               (Parser, char, sepBy, skipSpace, try)
import           Data.Morpheus.Parser.Primitive     (token)
import           Data.Morpheus.Types.Internal.Value (Value (..))
import           Data.Text                          (Text)

entry :: Parser Value -> Parser (Text, Value)
entry parser = do
  skipSpace
  key <- token
  skipSpace
  _ <- char ':'
  skipSpace
  value <- try parser
  skipSpace
  return (key, value)

entries :: Parser Value -> Parser [(Text, Value)]
entries parser = do
  _ <- char '{'
  skipSpace
  entries' <- entry parser `sepBy` char ','
  skipSpace
  _ <- char '}'
  return entries'

inputObject :: Parser Value -> Parser Value
inputObject parser = do
  skipSpace
  Object <$> entries parser
