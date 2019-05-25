module Data.Morpheus.Parser.Arguments
  ( maybeArguments
  ) where

import           Control.Applicative                           ((<|>))
import           Data.Attoparsec.Text                          (Parser, skipSpace)
import           Data.Morpheus.Parser.InputValues.Value        (parseValue)
import           Data.Morpheus.Parser.Primitive                (getPosition, token, variable)
import           Data.Morpheus.Parser.Terms                    (parseAssignment, parseMaybeTuple)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Argument (..), RawArgument (..), RawArguments,
                                                                Reference (..))
import           Data.Morpheus.Types.Internal.Value            (Value (Enum))

enum :: Parser Value
enum = Enum <$> token

argumentType :: Parser RawArgument
argumentType = do
  position' <- getPosition
  value' <- parseValue <|> enum
  pure $ RawArgument $ Argument {argumentValue = value', argumentPosition = position'}

variableType :: Parser RawArgument
variableType = do
  (reference', position') <- variable
  pure $ VariableReference $ Reference {referenceName = reference', referencePosition = position'}

inputValue :: Parser RawArgument
inputValue = skipSpace *> argumentType <|> variableType

maybeArguments :: Parser RawArguments
maybeArguments = parseMaybeTuple argument
  where
    argument = parseAssignment token inputValue
