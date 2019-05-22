module Data.Morpheus.Parser.InputValues.Value
  ( parseValue
  ) where

import           Control.Applicative                          ((<|>))
import           Data.Attoparsec.Text                         (Parser)
import           Data.Morpheus.Parser.InputValues.InputList   (inputList)
import           Data.Morpheus.Parser.InputValues.InputObject (inputObject)
import           Data.Morpheus.Parser.Primitive               (valueBoolean, valueNull, valueNumber, valueString)
import           Data.Morpheus.Types.Internal.Value           (Value)

parseValue :: Parser Value
parseValue =
  valueNull <|> valueBoolean <|> valueNumber <|> valueString <|> inputObject parseValue <|> inputList parseValue
