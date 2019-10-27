{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Body
  ( entries
  ) where

import           Data.Text                                     (Text)
import           Text.Megaparsec                               (label, try, (<|>))

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal       (Parser, getLocation)
import           Data.Morpheus.Parsing.Internal.Terms          (onType, parseAlias, setOf, spreadLiteral, token)
import           Data.Morpheus.Parsing.Request.Arguments       (maybeArguments)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelectionSet, Reference (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (Selection (..))

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
    position <- getLocation
    aliasName  <- parseAlias
    name <- token
    arguments <- maybeArguments
    value <- selSet aliasName arguments <|> buildField aliasName arguments position
    return (name, value)
  where
    ----------------------------------------
    buildField selectionAlias selectionArguments selectionPosition =
       pure (RawSelectionField $ Selection { selectionAlias , selectionArguments,  selectionRec = (), selectionPosition})
    -----------------------------------------
    selSet :: Maybe Text -> RawArguments -> Parser RawSelection
    selSet selectionAlias selectionArguments =
      label "body" $ do
        selectionPosition <- getLocation
        selectionRec <- entries
        return (RawSelectionSet $ Selection {selectionAlias , selectionArguments, selectionRec, selectionPosition})


entries :: Parser RawSelectionSet
entries = label "entries" $ setOf entry
  where
    entry = label "entry" $ try inlineFragment <|> try spread <|> parseSelectionField

