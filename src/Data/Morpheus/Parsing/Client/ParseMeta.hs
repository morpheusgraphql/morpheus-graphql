{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Parsing.Client.ParseMeta
  ( parseMeta
  ) where

import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, token)
import           Data.Text                               (Text)
import           Text.Megaparsec                         (between, label)
import           Text.Megaparsec.Char                    (char, space)

parseMeta :: Parser (Text, Text)
parseMeta =
  label "MetaData" $ do
    space
    _ <- char '#'
    between
      (char '{' *> space)
      (char '}' *> space)
      (parseAssignment token token)
