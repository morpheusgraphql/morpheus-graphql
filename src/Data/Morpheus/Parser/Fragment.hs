{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Fragment
  ( fragment
  ) where

import           Data.Attoparsec.Text                      (Parser, skipSpace, string)
import           Data.Morpheus.Parser.Body                 (entries)
import           Data.Morpheus.Parser.Primitive            (getPosition, token)
import           Data.Morpheus.Parser.Terms                (onType)
import           Data.Morpheus.Types.Internal.AST.Fragment (Fragment (..))
import           Data.Text                                 (Text)

fragment :: Parser (Text, Fragment)
fragment = do
  skipSpace
  index <- getPosition
  _ <- string "fragment"
  skipSpace
  name <- token
  targetName <- onType
  skipSpace
  fragmentBody <- entries
  pure (name, Fragment {key = name, target = targetName, content = fragmentBody, position = index})
