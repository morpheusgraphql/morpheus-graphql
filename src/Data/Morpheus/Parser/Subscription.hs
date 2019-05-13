{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Subscription
  ( subscription
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, skipSpace, string, try)
import           Data.Morpheus.Parser.Body          (entries)
import           Data.Morpheus.Parser.Primitive     (getPosition, token)
import           Data.Morpheus.Parser.RootHead      (rootHeadArguments)
import           Data.Morpheus.Types.Query.Operator (Operator (..), RawOperator)

subscription :: Parser RawOperator
subscription = do
  pos <- getPosition
  _ <- string "subscription "
  skipSpace
  name <- token
  variables <- try (skipSpace *> rootHeadArguments) <|> pure []
  skipSpace
  sel <- entries
  pure $ Subscription name variables sel pos
