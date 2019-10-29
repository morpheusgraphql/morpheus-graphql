{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Selection
  ( parseSelectionSet
  , parseFragmentDefinition
  ) where

import           Data.Text                                     (Text)
import           Text.Megaparsec                               (label, try, (<|>))

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal       (Parser, getLocation)
import           Data.Morpheus.Parsing.Internal.Pattern        (optionalDirectives)
import           Data.Morpheus.Parsing.Internal.Terms          (keyword, parseAlias, parseName, parseTypeCondition,
                                                                setOf, spreadLiteral, token)
import           Data.Morpheus.Parsing.Request.Arguments       (maybeArguments)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelectionSet, Reference (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (Selection (..))


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
    parseSelection = label "Selection" $
            try inlineFragment
        <|> try spread
        <|> parseSelectionField

-- Fields: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fields
--
-- Field
-- Alias(opt) Name Arguments(opt) Directives(opt) SelectionSet(opt)
--
parseSelectionField :: Parser (Text, RawSelection)
parseSelectionField =
  label "SelectionField" $ do
    position <- getLocation
    aliasName  <- parseAlias
    name <- parseName
    arguments <- maybeArguments
    -- TODO: handle Directives
    _directives <- optionalDirectives
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
        selectionRec <- parseSelectionSet
        return (RawSelectionSet $ Selection {selectionAlias , selectionArguments, selectionRec, selectionPosition})


--
-- Fragments: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fragments
--
--  FragmentName : Name
--

--  FragmentSpread
--    ...FragmentName Directives(opt)
--
spread :: Parser (Text, RawSelection)
spread =
  label "FragmentSpread" $ do
    referencePosition <- spreadLiteral
    referenceName <- token
    -- TODO: handle Directives
    _directives <- optionalDirectives
    return (referenceName, Spread $ Reference {referenceName, referencePosition})

-- FragmentDefinition : https://graphql.github.io/graphql-spec/June2018/#FragmentDefinition
--
--  FragmentDefinition:
--   fragment FragmentName TypeCondition Directives(opt) SelectionSet
--
parseFragmentDefinition :: Parser (Text, Fragment)
parseFragmentDefinition =
  label "FragmentDefinition" $ do
    keyword "fragment"
    fragmentPosition <- getLocation
    name <- parseName
    fragmentType <- parseTypeCondition
    -- TODO: handle Directives
    _directives <- optionalDirectives
    fragmentSelection <- parseSelectionSet
    pure (name, Fragment {fragmentType, fragmentSelection, fragmentPosition})

-- Inline Fragments : https://graphql.github.io/graphql-spec/June2018/#sec-Inline-Fragments
--
--  InlineFragment:
--  ... TypeCondition(opt) Directives(opt) SelectionSet
--
inlineFragment :: Parser (Text, RawSelection)
inlineFragment =
  label "InlineFragment" $ do
    fragmentPosition <- spreadLiteral
    -- TODO: optional
    fragmentType <- parseTypeCondition
    -- TODO: handle Directives
    _directives <- optionalDirectives
    fragmentSelection <- parseSelectionSet
    pure ("INLINE_FRAGMENT", InlineFragment $ Fragment {fragmentType, fragmentSelection, fragmentPosition})