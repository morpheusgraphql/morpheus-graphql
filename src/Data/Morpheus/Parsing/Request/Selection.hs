{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Morpheus.Parsing.Request.Selection
  ( parseSelectionSet
  , parseFragmentDefinition
  )
where

import           Text.Megaparsec                ( label
                                                , try
                                                , (<|>)
                                                )

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , getLocation
                                                )
import           Data.Morpheus.Parsing.Internal.Pattern
                                                ( optionalDirectives )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( keyword
                                                , parseAlias
                                                , parseName
                                                , parseTypeCondition
                                                , setOf
                                                , spreadLiteral
                                                , token
                                                )
import           Data.Morpheus.Parsing.Internal.Arguments
                                                ( maybeArguments )
import           Data.Morpheus.Types.Internal.AST
                                                ( Selection(..)
                                                , SelectionContent(..)
                                                , Ref(..)
                                                , Fragment(..)
                                                , RawArguments
                                                , RawSelection
                                                , RawSelectionSet
                                                , Name
                                                , Position
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
parseSelectionSet :: Parser RawSelectionSet
parseSelectionSet = label "SelectionSet" $ setOf parseSelection
 where
  parseSelection =
    label "Selection"
      $   try inlineFragment
      <|> try spread
      <|> parseSelectionField

-- Fields: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fields
--
-- Field
-- Alias(opt) Name Arguments(opt) Directives(opt) SelectionSet(opt)
--
parseSelectionField :: Parser RawSelection
parseSelectionField = label "SelectionField" $ do
  selectionPosition   <- getLocation
  selectionAlias      <- parseAlias
  selectionName       <- parseName
  selectionArguments  <- maybeArguments
  -- TODO: handle Directives
  _directives   <- optionalDirectives
  selSet selectionName selectionAlias selectionArguments <|> pure Selection { selectionContent   = SelectionField, ..}
 where
  -----------------------------------------
  selSet :: Name -> Maybe Name -> RawArguments -> Parser RawSelection
  selSet selectionName selectionAlias selectionArguments = label "body" $ do
    selectionPosition <- getLocation
    selectionSet      <- parseSelectionSet
    pure Selection { selectionContent   = SelectionSet selectionSet, ..}

--
-- Fragments: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fragments
--
--  FragmentName : Name
--

--  FragmentSpread
--    ...FragmentName Directives(opt)
--
spread :: Parser RawSelection
spread = label "FragmentSpread" $ do
  refPosition <- spreadLiteral
  refName     <- token
  -- TODO: handle Directives
  _directives <- optionalDirectives
  pure $ Spread Ref { .. }

-- FragmentDefinition : https://graphql.github.io/graphql-spec/June2018/#FragmentDefinition
--
--  FragmentDefinition:
--   fragment FragmentName TypeCondition Directives(opt) SelectionSet
--
parseFragmentDefinition :: Parser Fragment
parseFragmentDefinition = label "FragmentDefinition" $ do
  keyword "fragment"
  fragmentPosition  <- getLocation
  fragmentName      <- parseName
  fragmentBody fragmentName fragmentPosition

-- Inline Fragments : https://graphql.github.io/graphql-spec/June2018/#sec-Inline-Fragments
--
--  InlineFragment:
--  ... TypeCondition(opt) Directives(opt) SelectionSet
--
inlineFragment :: Parser RawSelection
inlineFragment = label "InlineFragment" $ do
  fragmentPosition  <- spreadLiteral
  InlineFragment <$> fragmentBody "INLINE_FRAGMENT" fragmentPosition

fragmentBody :: Name -> Position -> Parser Fragment
fragmentBody fragmentName fragmentPosition = label "FragmentBody" $ do
  fragmentType      <- parseTypeCondition
  -- TODO: handle Directives
  _directives       <- optionalDirectives
  fragmentSelection <- parseSelectionSet
  pure $ Fragment { .. }