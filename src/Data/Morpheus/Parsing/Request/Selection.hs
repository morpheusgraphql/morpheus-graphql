{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Morpheus.Parsing.Request.Selection
  ( parseSelectionSet
  , parseFragmentDefinition
  )
where

import           Data.Text                      ( Text )
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
                                                , collection
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
parseSelectionSet = label "SelectionSet" $ collection parseSelection
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
parseSelectionField :: Parser (Text, RawSelection)
parseSelectionField = label "SelectionField" $ do
  position    <- getLocation
  aliasName   <- parseAlias
  name        <- parseName
  arguments   <- maybeArguments
  -- TODO: handle Directives
  _directives <- optionalDirectives
  value       <-
    selSet aliasName arguments <|> buildField aliasName arguments position
  return (name, value)
 where
    ----------------------------------------
  buildField selectionAlias selectionArguments selectionPosition = pure
    (Selection { selectionAlias
               , selectionArguments
               , selectionContent   = SelectionField
               , selectionPosition
               }
    )
  -----------------------------------------
  selSet :: Maybe Text -> RawArguments -> Parser RawSelection
  selSet selectionAlias selectionArguments = label "body" $ do
    selectionPosition <- getLocation
    selectionSet      <- parseSelectionSet
    return
      (Selection { selectionAlias
                 , selectionArguments
                 , selectionContent   = SelectionSet selectionSet
                 , selectionPosition
                 }
      )


--
-- Fragments: https://graphql.github.io/graphql-spec/June2018/#sec-Language.Fragments
--
--  FragmentName : Name
--

--  FragmentSpread
--    ...FragmentName Directives(opt)
--
spread :: Parser (Text, RawSelection)
spread = label "FragmentSpread" $ do
  refPosition <- spreadLiteral
  refName     <- token
  -- TODO: handle Directives
  _directives <- optionalDirectives
  return (refName, Spread $ Ref { refName, refPosition })

-- FragmentDefinition : https://graphql.github.io/graphql-spec/June2018/#FragmentDefinition
--
--  FragmentDefinition:
--   fragment FragmentName TypeCondition Directives(opt) SelectionSet
--
parseFragmentDefinition :: Parser Fragment
parseFragmentDefinition = label "FragmentDefinition" $ do
  keyword "fragment"
  fragmentPosition  <- getLocation
  fragmentName              <- parseName
  fragmentType      <- parseTypeCondition
  -- TODO: handle Directives
  _directives       <- optionalDirectives
  fragmentSelection <- parseSelectionSet
  pure Fragment { .. }

-- Inline Fragments : https://graphql.github.io/graphql-spec/June2018/#sec-Inline-Fragments
--
--  InlineFragment:
--  ... TypeCondition(opt) Directives(opt) SelectionSet
--
inlineFragment :: Parser (Text, RawSelection)
inlineFragment = label "InlineFragment" $ do
  fragmentPosition  <- spreadLiteral
  -- TODO: optional
  fragmentType      <- parseTypeCondition
  -- TODO: handle Directives
  _directives       <- optionalDirectives
  fragmentSelection <- parseSelectionSet
  pure
    ( "INLINE_FRAGMENT"
    , InlineFragment
      $ Fragment { fragmentName = "INLINE_FRAGMENT", .. }
    )
