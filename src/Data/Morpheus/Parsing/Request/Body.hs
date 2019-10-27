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
import           Data.Morpheus.Parsing.Internal.Terms          (onType, parseNameWithAlias, qualifier, setOf,
                                                                spreadLiteral, token)
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
    (position , name, nonAliasName)  <- parseNameWithAlias
    arguments <- maybeArguments
    value <- selSet nonAliasName arguments <|> buildField nonAliasName arguments position
    return (name, value)
  where
    ----------------------------------------
    buildField selectionNonAliasName selectionArguments selectionPosition =
       pure (RawSelectionField $ Selection { selectionNonAliasName , selectionArguments,  selectionRec = (), selectionPosition})
    -----------------------------------------
    selSet :: Maybe Text -> RawArguments -> Parser RawSelection
    selSet selectionNonAliasName selectionArguments =
      label "body" $ do
        selectionPosition <- getLocation
        selectionRec <- entries
        return (RawSelectionSet $ Selection {selectionNonAliasName , selectionArguments, selectionRec, selectionPosition})


entries :: Parser RawSelectionSet
entries = label "entries" $ setOf entry
  where
    entry = label "entry" $ try inlineFragment <|> try spread <|> parseSelectionField

