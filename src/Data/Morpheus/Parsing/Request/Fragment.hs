{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Fragment
  ( fragment
  ) where

import           Data.Text                                     (Text)
import           Text.Megaparsec                               (getSourcePos, label)
import           Text.Megaparsec.Char                          (space, string)

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal       (Parser)
import           Data.Morpheus.Parsing.Internal.Terms          (onType, token)
import           Data.Morpheus.Parsing.Request.Body            (entries)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..))

fragment :: Parser (Text, Fragment)
fragment =
  label "fragment" $ do
    _ <- string "fragment"
    space
    index <- getSourcePos
    name' <- token
    type' <- onType
    fragmentBody <- entries
    pure (name', Fragment {fragmentType = type', fragmentSelection = fragmentBody, fragmentPosition = index})
