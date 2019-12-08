{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Request.Arguments
  ( maybeArguments
  )
where

import           Text.Megaparsec                ( label
                                                , (<|>)
                                                )

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , getLocation
                                                )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( parseAssignment
                                                , parseMaybeTuple
                                                , token
                                                , variable
                                                )
import           Data.Morpheus.Parsing.Internal.Value
                                                ( enumValue
                                                , parseRawValue
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( ValueOrigin(..)
                                                , Argument(..)
                                                , RawArgument
                                                , RawArguments
                                                , Ref(..)
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
valueArgument = label "valueArgument" $ do
  argumentPosition <- getLocation
  argumentValue    <- parseRawValue
  pure $ Argument { argumentValue, argumentOrigin = INLINE, argumentPosition }

variableArgument :: Parser RawArgument
variableArgument = label "variableArgument" $ do
  (refName, refPosition) <- variable
  pure $ VariableRef $ Ref { refName, refPosition }

maybeArguments :: Parser RawArguments
maybeArguments = label "maybeArguments" $ parseMaybeTuple argument
  where argument = parseAssignment token (valueArgument <|> variableArgument)
