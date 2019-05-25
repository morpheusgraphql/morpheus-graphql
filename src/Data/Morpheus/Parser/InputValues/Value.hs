module Data.Morpheus.Parser.InputValues.Value
  ( parseValue
  ) where

import           Control.Applicative                        ((<|>))
import           Data.Attoparsec.Text                       (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Parser.InputValues.InputList (inputList)
import           Data.Morpheus.Parser.Primitive             (token, valueBoolean, valueNull, valueNumber, valueString)
import           Data.Morpheus.Parser.Terms                 (parseAssignment)
import           Data.Morpheus.Types.Internal.Value         (Value (..))

parseValue :: Parser Value
parseValue = valueNull <|> valueBoolean <|> valueNumber <|> valueString <|> objectValue <|> inputList parseValue

objectValue :: Parser Value
objectValue = do
  skipSpace
  _ <- char '{'
  skipSpace
  entries' <- parseAssignment token parseValue `sepBy` char ','
  skipSpace
  _ <- char '}'
  return (Object entries')
