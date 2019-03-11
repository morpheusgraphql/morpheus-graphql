{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Mutation
  ( mutation
  ) where

import           Control.Applicative            ((<|>))
import           Data.Attoparsec.Text           (Parser, skipSpace, string, try)
import           Data.Morpheus.Parser.Body      (body)
import           Data.Morpheus.Parser.Primitive (token)
import           Data.Morpheus.Parser.RootHead  (rootHeadArguments)
import           Data.Morpheus.Types.Types      (Arguments (..),
                                                 GQLOperator (..),
                                                 QuerySelection (..))
import           Data.Text                      (Text)

mutation :: Parser GQLOperator
mutation = do
  string "mutation "
  skipSpace
  name <- token
  variables <- try (skipSpace *> rootHeadArguments) <|> pure []
  MutationOperator name <$> body variables
