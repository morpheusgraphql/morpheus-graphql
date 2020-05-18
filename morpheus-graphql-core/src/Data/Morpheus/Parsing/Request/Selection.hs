{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Parsing.Request.Selection
  ( parseSelectionSet,
    parseFragmentDefinition,
  )
where

--
-- MORPHEUS

import Data.Morpheus.Parsing.Internal.Arguments
  ( maybeArguments,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Pattern
  ( optionalDirectives,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( keyword,
    parseAlias,
    parseName,
    parseTypeCondition,
    setOf,
    spreadLiteral,
  )
import Data.Morpheus.Types.Internal.AST
  ( Arguments,
    Directives,
    FieldName,
    Fragment (..),
    Position,
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
  )
import Text.Megaparsec
  ( (<|>),
    label,
    try,
  )

-- Selection Sets : https://graphql.github.io/graphql-spec/June2018/#sec-Selection-Sets
--
-- SelectionSet:
--  { Selection(list) }
--
-- Selection:
--   Field
--   FragmentSpread
--   InlineFragment
--
parseSelectionSet :: Parser (SelectionSet RAW)
parseSelectionSet = label "SelectionSet" $ setOf parseSelection
  where
    parseSelection =
      label "Selection" $
        try inlineFragment
          <|> try spread
          <|> parseSelectionField

-- Fields: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fields
--
-- Field
-- Alias(opt) Name Arguments(opt) Directives(opt) SelectionSet(opt)
--
parseSelectionField :: Parser (Selection RAW)
parseSelectionField = label "SelectionField" $ do
  selectionPosition <- getLocation
  selectionAlias <- parseAlias
  selectionName <- parseName
  selectionArguments <- maybeArguments
  selectionDirectives <- optionalDirectives
  selSet selectionName selectionAlias selectionArguments selectionDirectives <|> pure Selection {selectionContent = SelectionField, ..}
  where
    -----------------------------------------
    selSet :: FieldName -> Maybe FieldName -> Arguments RAW -> Directives RAW -> Parser (Selection RAW)
    selSet selectionName selectionAlias selectionArguments selectionDirectives = label "body" $ do
      selectionPosition <- getLocation
      selectionSet <- parseSelectionSet
      pure Selection {selectionContent = SelectionSet selectionSet, ..}

--
-- Fragments: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fragments
--
--  FragmentName : Name
--

--  FragmentSpread
--    ...FragmentName Directives(opt)
--
spread :: Parser (Selection RAW)
spread = label "FragmentSpread" $ do
  refPosition <- spreadLiteral
  refName <- parseName
  directives <- optionalDirectives
  pure $ Spread directives Ref {..}

-- FragmentDefinition : https://graphql.github.io/graphql-spec/June2018/#FragmentDefinition
--
--  FragmentDefinition:
--   fragment FragmentName TypeCondition Directives(opt) SelectionSet
--
parseFragmentDefinition :: Parser Fragment
parseFragmentDefinition = label "FragmentDefinition" $ do
  keyword "fragment"
  fragmentPosition <- getLocation
  fragmentName <- parseName
  fragmentBody fragmentName fragmentPosition

-- Inline Fragments : https://graphql.github.io/graphql-spec/June2018/#sec-Inline-Fragments
--
--  InlineFragment:
--  ... TypeCondition(opt) Directives(opt) SelectionSet
--
inlineFragment :: Parser (Selection RAW)
inlineFragment = label "InlineFragment" $ do
  fragmentPosition <- spreadLiteral
  InlineFragment <$> fragmentBody "INLINE_FRAGMENT" fragmentPosition

fragmentBody :: FieldName -> Position -> Parser Fragment
fragmentBody fragmentName fragmentPosition = label "FragmentBody" $ do
  fragmentType <- parseTypeCondition
  fragmentDirectives <- optionalDirectives
  fragmentSelection <- parseSelectionSet
  pure $ Fragment {..}
