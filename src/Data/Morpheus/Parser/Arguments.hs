module Data.Morpheus.Parser.Arguments
  ( maybeArguments
  ) where

import           Control.Applicative                           ((<|>))
import           Data.Attoparsec.Text                          (Parser)
import           Data.Morpheus.Parser.Internal                 (getPosition)
import           Data.Morpheus.Parser.Primitive                (token, variable)
import           Data.Morpheus.Parser.Terms                    (parseAssignment, parseMaybeTuple)
import           Data.Morpheus.Parser.Value                    (enumValue, parseValue)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Argument (..), RawArgument (..), RawArguments,
                                                                Reference (..))

valueArgument :: Parser RawArgument
valueArgument = do
  position' <- getPosition
  value' <- parseValue <|> enumValue
  pure $ RawArgument $ Argument {argumentValue = value', argumentPosition = position'}

variableArgument :: Parser RawArgument
variableArgument = do
  (reference', position') <- variable
  pure $ VariableReference $ Reference {referenceName = reference', referencePosition = position'}

maybeArguments :: Parser RawArguments
maybeArguments = parseMaybeTuple argument
  where
    argument = parseAssignment token (valueArgument <|> variableArgument)
