{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Query
  ( query
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, skipSpace, string, try)
import           Data.Morpheus.Parser.Body          (entries)
import           Data.Morpheus.Parser.Primitive     (getPosition, token)
import           Data.Morpheus.Parser.RootHead      (rootHeadArguments)
import           Data.Morpheus.Types.Query.Operator (Operator (..))
import           Data.Morpheus.Types.Types          (Arguments)
import           Data.Text                          (Text)

queryHead :: Parser (Text, Arguments)
queryHead = do
  _ <- string "query "
  skipSpace
  queryName <- token
  variables <- try (skipSpace *> rootHeadArguments) <|> pure []
  pure (queryName, variables)

query :: Parser Operator
query = do
  pos <- getPosition
  (queryName, args) <- try (skipSpace *> queryHead) <|> pure ("", [])
  selection <- entries
  pure $ Query queryName args selection pos
