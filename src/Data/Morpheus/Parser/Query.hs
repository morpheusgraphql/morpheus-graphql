{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Query
  ( query
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, skipSpace, try)
import           Data.Morpheus.Parser.Body          (entries)
import           Data.Morpheus.Parser.Operator      (operatorHead)
import           Data.Morpheus.Parser.Primitive     (getPosition)
import           Data.Morpheus.Types.Query.Operator (Operator (..), RawOperator)

query :: Parser RawOperator
query = do
  pos <- getPosition
  (queryName, args) <- try (operatorHead "query") <|> pure ("", [])
  skipSpace
  selection <- entries
  pure $ Query queryName args selection pos
