{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Fragment
  ( fragment
  ) where

import           Data.Attoparsec.Text                          (Parser, skipSpace, string)
import           Data.Morpheus.Parser.Body                     (entries)
import           Data.Morpheus.Parser.Internal                 (getPosition)
import           Data.Morpheus.Parser.Primitive                (token)
import           Data.Morpheus.Parser.Terms                    (onType)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..))
import           Data.Text                                     (Text)

fragment :: Parser (Text, Fragment)
fragment = do
  skipSpace
  index <- getPosition
  _ <- string "fragment"
  skipSpace
  name' <- token
  type' <- onType
  skipSpace
  fragmentBody <- entries
  pure (name', Fragment {fragmentType = type', fragmentSelection = fragmentBody, fragmentPosition = index})
