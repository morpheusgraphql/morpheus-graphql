module Data.Morpheus.Parser.InputValues.InputList
  ( inputList
  ) where

import           Data.Attoparsec.Text               (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Types.Internal.Value (Value (..))

inputList :: Parser Value -> Parser Value
inputList parser = do
  skipSpace
  _ <- char '['
  skipSpace
  entries' <-
    (do skipSpace
        val <- parser
        skipSpace
        return val) `sepBy`
    char ','
  skipSpace
  _ <- char ']'
  return (JSList entries')
