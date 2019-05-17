{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Mutation
  ( mutation
  ) where

import           Data.Attoparsec.Text               (Parser, skipSpace)
import           Data.Morpheus.Parser.Body          (entries)
import           Data.Morpheus.Parser.Operator      (operatorHead)
import           Data.Morpheus.Parser.Primitive     (getPosition)
import           Data.Morpheus.Types.Query.Operator (Operator (..), RawOperator)

mutation :: Parser RawOperator
mutation = do
  pos <- getPosition
  (name, variables) <- operatorHead "mutation"
  skipSpace
  sel <- entries
  pure $ Mutation name variables sel pos
