{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Arguments
  ( maybeArguments,
    parseArgumentsOpt,
  )
where

-- MORPHEUS
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( parseAssignment,
    token,
    uniqTupleOpt,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( parseRawValue,
    parseValue,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    RAW,
    VALID,
  )
import Text.Megaparsec (label)

-- Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- Arguments[Const]
-- ( Argument[Const](list) )
--
-- Argument[Const]
--  Name : Value[Const]
valueArgument :: Parser (Argument RAW)
valueArgument =
  label "Argument" $ do
    argumentPosition <- getLocation
    (argumentName, argumentValue) <- parseAssignment token parseRawValue
    pure $ Argument {argumentName, argumentValue, argumentPosition}

parseArgument :: Parser (Argument VALID)
parseArgument =
  label "Argument" $ do
    argumentPosition <- getLocation
    (argumentName, argumentValue) <- parseAssignment token parseValue
    pure $ Argument {argumentName, argumentValue, argumentPosition}

parseArgumentsOpt :: Parser (Arguments VALID)
parseArgumentsOpt =
  label "Arguments" $
    uniqTupleOpt parseArgument

maybeArguments :: Parser (Arguments RAW)
maybeArguments =
  label "Arguments" $
    uniqTupleOpt valueArgument
