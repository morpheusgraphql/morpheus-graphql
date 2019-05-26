{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Control.Applicative                           ((<|>))
import           Data.Attoparsec.Text                          (Parser, char, sepBy, skipSpace)
import           Data.Char                                     (isAlpha)
import           Data.Morpheus.Parser.Arguments                (maybeArguments)
import           Data.Morpheus.Parser.Internal                 (getPosition, syntaxFail)
import           Data.Morpheus.Parser.Primitive                (qualifier, token)
import           Data.Morpheus.Parser.Terms                    (lookAheadChar, onType, parseAssignment, parseChar,
                                                                parseWhenChar, spreadLiteral)
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
parseSelectionField :: Parser (Text, RawSelection)
parseSelectionField = do
  (name', position') <- qualifier
  arguments' <- maybeArguments
  value' <- parseWhenChar '{' (body arguments') (buildField arguments' position')
  return (name', value')
  where
    buildField arguments' position' =
      pure
        (RawSelectionField $
         RawSelection' {rawSelectionArguments = arguments', rawSelectionRec = (), rawSelectionPosition = position'})

alias :: Parser (Text, RawSelection)
alias = do
  ((name', position'), selection') <- parseAssignment qualifier parseSelectionField
  return (name', RawAlias {rawAliasPosition = position', rawAliasSelection = selection'})

--isSep :: Char -> Bool
--isSep = (`elem` [',', ' ', '\n', '\t'])
bodySeparator :: Parser Char
bodySeparator = char ',' <|> char ' ' <|> char '\n' <|> char '\t'

entries :: Parser RawSelectionSet
entries = do
  parseChar '{'
  skipSpace
  entries' <- entry `sepBy` bodySeparator
  skipSpace
  parseChar '}'
  return entries'
  where
    entry = do
      char' <- lookAheadChar
      case char' of
        '.' -> inlineFragment <|> spread
        ch'
          | isAlpha ch' || ch' == '_' -> alias <|> parseSelectionField
        ch' -> syntaxFail (pack $ "unknown Character on selection: \"" ++ [ch'] ++ "\"")

body :: RawArguments -> Parser RawSelection
body args = do
  skipSpace
  index <- getPosition
  entries' <- entries
  return
    (RawSelectionSet $
     RawSelection' {rawSelectionArguments = args, rawSelectionRec = entries', rawSelectionPosition = index})
