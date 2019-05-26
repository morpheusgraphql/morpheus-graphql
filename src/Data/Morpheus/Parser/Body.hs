{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Control.Applicative                           ((<|>))
import           Data.Attoparsec.Text                          (Parser, char, sepBy, skipSpace, try)
import           Data.Char                                     (isAlpha)
import           Data.Morpheus.Parser.Arguments                (maybeArguments)
import           Data.Morpheus.Parser.Internal                 (getPosition, syntaxFail)
import           Data.Morpheus.Parser.Primitive                (qualifier, separator, token)
import           Data.Morpheus.Parser.Terms                    (lookAheadChar, onType, parseAssignment, spreadLiteral)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet, Reference (..))
import           Data.Text                                     (Text, pack)

spread :: Parser (Text, RawSelection)
spread = do
  index <- spreadLiteral
  skipSpace
  key' <- token
  return (key', Spread $ Reference {referenceName = key', referencePosition = index})

inlineFragment :: Parser (Text, RawSelection)
inlineFragment = do
  index <- spreadLiteral
  type' <- onType
  skipSpace
  fragmentBody <- entries
  pure
    ( "INLINE_FRAGMENT"
    , InlineFragment $ Fragment {fragmentType = type', fragmentSelection = fragmentBody, fragmentPosition = index})

{-
  accept:
  - field
  - field {...}
  - field (...)
  - field () {...}
-}
selection :: Parser (Text, RawSelection)
selection = do
  (name', position') <- qualifier
  arguments' <- maybeArguments
  value <- try (body arguments') <|> buildField arguments' position'
  return (name', value)

buildField :: RawArguments -> Int -> Parser RawSelection
buildField arguments' position' =
  pure
    (RawSelectionField $
     RawSelection' {rawSelectionArguments = arguments', rawSelectionRec = (), rawSelectionPosition = position'})

{--
  accept:
    field1: field(a:320)
    field2: field (a:640)
    field3: field
--}
alias :: Parser (Text, RawSelection)
alias = do
  ((name', position'), selection') <- parseAssignment qualifier selection
  return (name', RawAlias {rawAliasPosition = position', rawAliasSelection = selection'})

separated :: Parser a -> Parser [a]
separated x = x `sepBy` separator

entries :: Parser RawSelectionSet
entries = do
  _ <- char '{'
  skipSpace
  entries' <- separated entry
  skipSpace
  _ <- char '}'
  return entries'
  where
    entry = do
      char' <- lookAheadChar
      case char' of
        '.' -> inlineFragment <|> spread
        ch'
          | isAlpha ch' || ch' == '_' -> alias <|> selection
        ch' -> syntaxFail (pack $ "unknown Character on selection: \"" ++ [ch'] ++ "\"")

body :: RawArguments -> Parser RawSelection
body args = do
  skipSpace
  index <- getPosition
  entries' <- entries
  return
    (RawSelectionSet $
     RawSelection' {rawSelectionArguments = args, rawSelectionRec = entries', rawSelectionPosition = index})
