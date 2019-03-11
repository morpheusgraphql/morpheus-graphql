{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Fragment
  ( fragment
  ) where

import           Data.Attoparsec.Text           (Parser, skipSpace, string)
import           Data.Morpheus.Parser.Body      (body)
import           Data.Morpheus.Parser.Primitive (token)
import           Data.Morpheus.Types.Types      (Fragment (..))
import           Data.Text                      (Text)

fragment :: Parser (Text, Fragment)
fragment = do
  skipSpace
  _ <- string "fragment"
  skipSpace
  name <- token
  skipSpace
  _ <- string "on"
  skipSpace
  targetName <- token
  skipSpace
  fragmentBody <- body []
  pure (name, Fragment name targetName fragmentBody)
