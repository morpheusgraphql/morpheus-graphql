{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Subscription
  ( subscription
  ) where

import           Data.Attoparsec.Text                      (Parser, skipSpace)
import           Data.Morpheus.Parser.Body                 (entries)
import           Data.Morpheus.Parser.Operator             (operatorHead)
import           Data.Morpheus.Parser.Primitive            (getPosition)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..), RawOperator)

subscription :: Parser RawOperator
subscription = do
  pos <- getPosition
  (name, variables) <- operatorHead "subscription"
  skipSpace
  sel <- entries
  pure $ Subscription (Operator' name variables sel pos)
