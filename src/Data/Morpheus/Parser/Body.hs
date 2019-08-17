{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( entries
  ) where

import           Data.Morpheus.Parser.Arguments                (maybeArguments)
import           Data.Morpheus.Parsing.Internal.Internal       (Parser)
import           Data.Morpheus.Parsing.Internal.Terms          (onType, parseAssignment, qualifier, spreadLiteral,
                                                                token)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet, Reference (..))
import           Data.Text                                     (Text)
import           Text.Megaparsec                               (between, getSourcePos, label, many, sepEndBy, try,
                                                                (<|>))
import           Text.Megaparsec.Char                          (char, space)

spread :: Parser (Text, RawSelection)
spread =
  label "spread" $ do
    index <- spreadLiteral
    key' <- token
    return (key', Spread $ Reference {referenceName = key', referencePosition = index})

inlineFragment :: Parser (Text, RawSelection)
inlineFragment =
  label "InlineFragment" $ do
    index <- spreadLiteral
    type' <- onType
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
parseSelectionField =
  label "SelectionField" $ do
    (name', position') <- qualifier
    arguments' <- maybeArguments
    value' <- body arguments' <|> buildField arguments' position'
    return (name', value')
  where
    buildField arguments' position' =
      pure
        (RawSelectionField $
         RawSelection' {rawSelectionArguments = arguments', rawSelectionRec = (), rawSelectionPosition = position'})

alias :: Parser (Text, RawSelection)
alias =
  label "alias" $ do
    ((name', position'), selection') <- parseAssignment qualifier parseSelectionField
    return (name', RawAlias {rawAliasPosition = position', rawAliasSelection = selection'})

entries :: Parser RawSelectionSet
entries = label "entries" $ between (char '{' *> space) (char '}' *> space) (entry `sepEndBy` many (char ',' *> space))
  where
    entry = label "entry" $ try inlineFragment <|> try spread <|> try alias <|> parseSelectionField

body :: RawArguments -> Parser RawSelection
body args =
  label "body" $ do
    index <- getSourcePos
    entries' <- entries
    return
      (RawSelectionSet $
       RawSelection' {rawSelectionArguments = args, rawSelectionRec = entries', rawSelectionPosition = index})
