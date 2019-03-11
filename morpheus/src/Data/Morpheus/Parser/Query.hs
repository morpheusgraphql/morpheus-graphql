{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Query
  ( query
  ) where

import           Control.Applicative            ((<|>))
import           Data.Attoparsec.Text           (Parser, skipSpace, string, try)
import           Data.Morpheus.Parser.Body      (body)
import           Data.Morpheus.Parser.Primitive (token)
import           Data.Morpheus.Parser.RootHead  (rootHeadArguments)
import           Data.Morpheus.Types.Types      (Arguments, GQLOperator (..))
import           Data.Text                      (Text)

queryHead :: Parser (Text, Arguments)
queryHead = do
  _ <- string "query "
  skipSpace
  queryName <- token
  variables <- try (skipSpace *> rootHeadArguments) <|> pure []
  pure (queryName, variables)

query :: Parser GQLOperator
query = do
  (queryName, args) <- try (skipSpace *> queryHead) <|> pure ("", [])
  selection <- body args
  pure $ QueryOperator queryName selection
