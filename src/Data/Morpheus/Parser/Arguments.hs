module Data.Morpheus.Parser.Arguments
  ( maybeArguments
  ) where

import           Data.Morpheus.Parser.Internal                 (Parser)
import           Data.Morpheus.Parser.Primitive                (token, variable)
import           Data.Morpheus.Parser.Terms                    (parseAssignment, parseMaybeTuple)
import           Data.Morpheus.Parser.Value                    (enumValue, parseValue)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Argument (..), RawArgument (..), RawArguments,
                                                                Reference (..))
import           Text.Megaparsec                               (getSourcePos, label, (<|>))

valueArgument :: Parser RawArgument
valueArgument = label "valueArgument" $ do
  position' <- getSourcePos
  value' <- parseValue <|> enumValue
  pure $ RawArgument $ Argument {argumentValue = value', argumentPosition = position'}

variableArgument :: Parser RawArgument
variableArgument = label "variableArgument" $ do
  (reference', position') <- variable
  pure $ VariableReference $ Reference {referenceName = reference', referencePosition = position'}

maybeArguments :: Parser RawArguments
maybeArguments = label "maybeArguments" $ parseMaybeTuple argument
  where
    argument = parseAssignment token (valueArgument <|> variableArgument)
