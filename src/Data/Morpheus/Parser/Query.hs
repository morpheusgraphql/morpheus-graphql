{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Query
  ( query
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, skipSpace, try)
import           Data.Morpheus.Parser.Body          (entries)
import           Data.Morpheus.Parser.Operator      (operatorHead)
import           Data.Morpheus.Parser.Primitive     (getPosition)
import           Data.Morpheus.Types.Query.Operator (Operator (..), Operator' (..), RawOperator)

query :: Parser RawOperator
query = do
  skipSpace
  pos <- getPosition
  (name, args) <- try (operatorHead "query") <|> pure ("", [])
  skipSpace
  selection <- entries
  pure $ Query (Operator' name args selection pos)
