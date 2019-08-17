{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Fragment
  ( fragment
  ) where

import           Data.Morpheus.Parser.Body                     (entries)
import           Data.Morpheus.Parsing.Internal.Internal       (Parser)
import           Data.Morpheus.Parsing.Internal.Primitive      (token)
import           Data.Morpheus.Parsing.Internal.Terms          (onType)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..))
import           Data.Text                                     (Text)
import           Text.Megaparsec                               (getSourcePos, label)
import           Text.Megaparsec.Char                          (space, string)

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
