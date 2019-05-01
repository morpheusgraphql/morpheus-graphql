module Data.Morpheus.Parser.InputValues.InputList
  ( inputList
  ) where

import           Data.Attoparsec.Text       (Parser, char, sepBy, skipSpace)
import           Data.Morpheus.Types.JSType (JSType (..))

inputList :: Parser JSType -> Parser JSType
inputList parser = do
  skipSpace
  _ <- char '['
  skipSpace
  entries' <- parser `sepBy` char ','
  skipSpace
  _ <- char ']'
  return (JSList entries')
