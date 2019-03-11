{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Query
  ( query
  ) where

import           Control.Applicative            (many, some, (<|>))
import           Data.Attoparsec.Text           (IResult (Done), Parser, char,
                                                 endOfInput, letter, parse,
                                                 parseOnly, sepBy, skipSpace,
                                                 string, try)
import           Data.Data                      (Data)
import           Data.Morpheus.Parser.Body      (body)
import           Data.Morpheus.Parser.Primitive (token)
import           Data.Morpheus.Parser.RootHead  (rootHeadArguments)
import           Data.Morpheus.Types.Error      (GQLError)
import           Data.Morpheus.Types.Types      (Arguments (..),
                                                 GQLOperator (..),
                                                 QuerySelection (..),
                                                 SelectionSet)
import           Data.Text                      (Text)

queryHead :: Parser (Text, Arguments)
queryHead = do
  string "query "
  skipSpace
  queryName <- token
  variables <- try (skipSpace *> rootHeadArguments) <|> pure []
  pure (queryName, variables)

query :: Parser GQLOperator
query = do
  (queryName, args) <- try (skipSpace *> queryHead) <|> pure ("", [])
  selection <- body args
  pure $ QueryOperator queryName selection
