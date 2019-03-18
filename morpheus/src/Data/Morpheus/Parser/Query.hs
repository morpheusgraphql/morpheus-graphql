{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Query
  ( query
  ) where

import           Control.Applicative                    ((<|>))
import           Data.Attoparsec.Text                   (Parser, skipSpace, string, try)
import           Data.Morpheus.Parser.Body              (entries)
import           Data.Morpheus.Parser.Primitive         (getPosition, token)
import           Data.Morpheus.Parser.RootHead          (rootHeadArguments)
import           Data.Morpheus.Types.Query.Operator     (Operator (..), RawOperator)
import           Data.Morpheus.Types.Query.RawSelection (RawArguments)
import           Data.Text                              (Text)

queryHead :: Parser (Text, RawArguments)
queryHead = do
  _ <- string "query "
  skipSpace
  queryName <- token
  variables <- try (skipSpace *> rootHeadArguments) <|> pure []
  pure (queryName, variables)

query :: Parser RawOperator
query = do
  pos <- getPosition
  (queryName, args) <- try (skipSpace *> queryHead) <|> pure ("", [])
  skipSpace
  selection <- entries
  pure $ Query queryName args selection pos
