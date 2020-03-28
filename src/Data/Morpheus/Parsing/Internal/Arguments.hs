{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Arguments
  ( maybeArguments, 
    parseArgumentsOpt
  )
where

import           Text.Megaparsec                ( label)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , getLocation
                                                )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( parseAssignment
                                                , uniqTupleOpt
                                                , token
                                                )
import           Data.Morpheus.Parsing.Internal.Value
                                                ( parseRawValue 
                                                , parseValue
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Argument(..)
                                                , RawArgument
                                                , RawArguments
                                                , ValidArgument
                                                , OrderedMap
                                                )


-- Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- Arguments[Const]
-- ( Argument[Const](list) )
--
-- Argument[Const]
--  Name : Value[Const]
valueArgument :: Parser RawArgument
valueArgument = 
  label "Argument" $ do
    argumentPosition <- getLocation
    (argumentName, argumentValue )<- parseAssignment token parseRawValue
    pure $ Argument { argumentName, argumentValue, argumentPosition }

parseArgument :: Parser ValidArgument
parseArgument = 
  label "Argument" $ do
    argumentPosition <- getLocation
    (argumentName, argumentValue )<- parseAssignment token parseValue
    pure $ Argument { argumentName, argumentValue, argumentPosition }

parseArgumentsOpt :: Parser (OrderedMap ValidArgument)
parseArgumentsOpt = 
  label "Arguments" 
    $ uniqTupleOpt parseArgument

maybeArguments :: Parser RawArguments
maybeArguments = 
  label "Arguments" 
    $ uniqTupleOpt valueArgument
