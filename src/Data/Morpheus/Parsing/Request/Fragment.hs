{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Fragment
  ( fragment
  ) where

import           Data.Text                                     (Text)
import           Text.Megaparsec                               (label)
import           Text.Megaparsec.Char                          (space, string)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal       (Parser, getLocation)
import           Data.Morpheus.Parsing.Internal.Terms          (onType, token)
import           Data.Morpheus.Parsing.Request.Body            (entries)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..))

fragment :: Parser (Text, Fragment)
fragment =
  label "fragment" $ do
    _ <- string "fragment"
    space
    fragmentPosition <- getLocation
    name <- token
    fragmentType <- onType
    fragmentSelection <- entries
    pure (name, Fragment {fragmentType, fragmentSelection, fragmentPosition})
