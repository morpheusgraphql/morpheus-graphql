{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Arguments (maybeArguments) where

-- MORPHEUS
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( assignedFieldName,
    uniqTupleOpt,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( Parse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    Value,
  )
import Text.Megaparsec (label)

-- Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- Arguments[Const]
-- ( Argument[Const](list) )
--
-- Argument[Const]
--  Name : Value[Const]
valueArgument :: Parse (Value s) => Parser (Argument s)
valueArgument =
  label "Argument" $
    Argument
      <$> getLocation
      <*> assignedFieldName
      <*> parse

maybeArguments :: Parse (Value s) => Parser (Arguments s)
maybeArguments =
  label "Arguments" $
    uniqTupleOpt valueArgument
