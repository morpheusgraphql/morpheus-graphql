module Data.Morpheus.Parser.Value
  ( parseValue
  , enumValue
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.Primitive     (token, valueBoolean, valueNull, valueNumber, valueString)
import           Data.Morpheus.Parser.Terms         (parseAssignment)
import           Data.Morpheus.Types.Internal.Value (Value (..))

parseValue :: Parser Value
parseValue = valueNull <|> valueBoolean <|> valueNumber <|> valueString <|> objectValue <|> listValue

enumValue :: Parser Value
enumValue = Enum <$> token

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
