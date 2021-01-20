{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Arguments (maybeArguments) where

-- MORPHEUS
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( Term,
    fieldNameColon,
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
import Relude
import Text.Megaparsec (Stream, label)

-- Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- Arguments[Const]
-- ( Argument[Const](list) )
--
-- Argument[Const]
--  Name : Value[Const]
valueArgument :: (Term str, Stream str, Parse str (Value s)) => Parser str (Argument s)
valueArgument =
  label "Argument" $
    Argument
      <$> getLocation
      <*> fieldNameColon
      <*> parse
{-# INLINEABLE valueArgument #-}

maybeArguments :: (Term str, Stream str) => Parse str (Value s) => Parser str (Arguments s)
maybeArguments =
  label "Arguments" $
    uniqTupleOpt valueArgument
{-# INLINEABLE maybeArguments #-}
