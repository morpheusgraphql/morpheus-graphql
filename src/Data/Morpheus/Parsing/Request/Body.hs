{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Body
  ( entries
  ) where

import           Data.Text                                     (Text)
import           Text.Megaparsec                               (getSourcePos, label, try, (<|>))

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal       (Parser)
import           Data.Morpheus.Parsing.Internal.Terms          (onType, parseAssignment, qualifier, setOf,
                                                                spreadLiteral, token)
import           Data.Morpheus.Parsing.Request.Arguments       (maybeArguments)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet, Reference (..))

spread :: Parser (Text, RawSelection)
spread =
  label "spread" $ do
    referencePosition <- spreadLiteral
    referenceName <- token
    return (referenceName, Spread $ Reference {referenceName, referencePosition})

inlineFragment :: Parser (Text, RawSelection)
inlineFragment =
  label "InlineFragment" $ do
    fragmentPosition <- spreadLiteral
    fragmentType <- onType
    fragmentSelection <- entries
    pure ("INLINE_FRAGMENT", InlineFragment $ Fragment {fragmentType, fragmentSelection, fragmentPosition})

{-
  accept:
  - field
  - field {...}
  - field (...)
  - field () {...}
-}
parseSelectionField :: Parser (Text, RawSelection)
parseSelectionField =
  label "SelectionField" $ do
    (name, position) <- qualifier
    arguments <- maybeArguments
    value <- body arguments <|> buildField arguments position
    return (name, value)
  where
    buildField rawSelectionArguments rawSelectionPosition =
      pure (RawSelectionField $ RawSelection' {rawSelectionArguments, rawSelectionRec = (), rawSelectionPosition})

alias :: Parser (Text, RawSelection)
alias =
  label "alias" $ do
    ((name, rawAliasPosition), rawAliasSelection) <- parseAssignment qualifier parseSelectionField
    return (name, RawAlias {rawAliasPosition, rawAliasSelection})

entries :: Parser RawSelectionSet
entries = label "entries" $ setOf entry
  where
    entry = label "entry" $ try inlineFragment <|> try spread <|> try alias <|> parseSelectionField

body :: RawArguments -> Parser RawSelection
body rawSelectionArguments =
  label "body" $ do
    rawSelectionPosition <- getSourcePos
    rawSelectionRec <- entries
    return (RawSelectionSet $ RawSelection' {rawSelectionArguments, rawSelectionRec, rawSelectionPosition})
