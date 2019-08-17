{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Request.Arguments
  ( maybeArguments
  ) where

import           Data.Morpheus.Parsing.Internal.Internal       (Parser)
import           Data.Morpheus.Parsing.Internal.Terms          (parseAssignment, parseMaybeTuple, token, variable)
import           Data.Morpheus.Parsing.Request.Value           (enumValue, parseValue)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Argument (..), RawArgument (..), RawArguments,
                                                                Reference (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (ArgumentOrigin (..))
import           Text.Megaparsec                               (getSourcePos, label, (<|>))

valueArgument :: Parser RawArgument
valueArgument =
  label "valueArgument" $ do
    argumentPosition <- getSourcePos
    argumentValue <- parseValue <|> enumValue
    pure $ RawArgument $ Argument {argumentValue, argumentOrigin = INLINE, argumentPosition}

variableArgument :: Parser RawArgument
variableArgument =
  label "variableArgument" $ do
    (referenceName, referencePosition) <- variable
    pure $ VariableReference $ Reference {referenceName, referencePosition}

maybeArguments :: Parser RawArguments
maybeArguments = label "maybeArguments" $ parseMaybeTuple argument
  where
    argument = parseAssignment token (valueArgument <|> variableArgument)
