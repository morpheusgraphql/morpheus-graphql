{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Fragment
  ( fragment
  ) where

import           Control.Applicative            (many, some, (<|>))
import           Data.Attoparsec.Text           (IResult (Done), Parser, char,
                                                 endOfInput, letter, parse,
                                                 parseOnly, sepBy, skipSpace,
                                                 string, try)
import           Data.Morpheus.Parser.Body      (body)
import           Data.Morpheus.Parser.Primitive (token)
import           Data.Morpheus.Types.Types      (Fragment (..))
import           Data.Text                      (Text (..), pack, unpack)

fragment :: Parser (Text, Fragment)
fragment = do
  skipSpace
  string "fragment"
  skipSpace
  name <- token
  skipSpace
  string "on"
  skipSpace
  targetName <- token
  skipSpace
  fragmentBody <- body []
  pure (name, Fragment name targetName fragmentBody)
