{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Mutation
  ( mutation
  ) where

import           Control.Applicative                ((<|>))
import           Data.Attoparsec.Text               (Parser, skipSpace, string, try)
import           Data.Morpheus.Parser.Body          (entries)
import           Data.Morpheus.Parser.Primitive     (getPosition, token)
import           Data.Morpheus.Parser.RootHead      (rootHeadArguments)
import           Data.Morpheus.Types.Query.Operator (Operator (..))

mutation :: Parser Operator
mutation = do
  pos <- getPosition
  _ <- string "mutation "
  skipSpace
  name <- token
  variables <- try (skipSpace *> rootHeadArguments) <|> pure []
  sel <- entries
  pure $ Mutation name variables sel pos
