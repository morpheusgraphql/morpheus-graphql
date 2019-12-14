{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Request.Arguments
  ( maybeArguments
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
                                                , parseMaybeTuple
                                                , token
                                                )
import           Data.Morpheus.Parsing.Internal.Value
                                                ( parseRawValue )
import           Data.Morpheus.Types.Internal.AST
                                                ( Argument(..)
                                                , RawArgument
                                                , RawArguments
                                                )


-- Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- Arguments[Const]
-- ( Argument[Const](list) )
--
-- Argument[Const]
--  Name : Value[Const]
-- TODO: move variable to Value
valueArgument :: Parser RawArgument
valueArgument = label "Argument" $ do
  argumentPosition <- getLocation
  argumentValue    <- parseRawValue
  pure $ Argument { argumentValue, argumentPosition }

maybeArguments :: Parser RawArguments
maybeArguments =
  label "Arguments" $ parseMaybeTuple (parseAssignment token valueArgument)
