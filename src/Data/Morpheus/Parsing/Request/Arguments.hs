{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Request.Arguments
  ( maybeArguments
  ) where

import           Text.Megaparsec                               (label, (<|>))

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal       (Parser, getLocation)
import           Data.Morpheus.Parsing.Internal.Terms          (parseAssignment, parseMaybeTuple, token, variable)
import           Data.Morpheus.Parsing.Internal.Value          (enumValue, parseValue)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Argument (..), RawArgument (..), RawArguments)
import           Data.Morpheus.Types.Internal.AST.Selection    (ArgumentOrigin (..))
import           Data.Morpheus.Types.Internal.Base             (Reference (..))

valueArgument :: Parser RawArgument
valueArgument =
  label "valueArgument" $ do
    argumentPosition <- getLocation
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
