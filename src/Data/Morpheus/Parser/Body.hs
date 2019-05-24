{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Body
  ( body
  , entries
  ) where

import           Control.Applicative                           ((<|>))
import           Data.Attoparsec.Text                          (Parser, char, sepBy, skipSpace, try)
import           Data.Morpheus.Parser.Arguments                (arguments)
import           Data.Morpheus.Parser.Primitive                (getPosition, qualifier, separator, token)
import           Data.Morpheus.Parser.Terms                    (onType, spreadLiteral)
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), RawArguments, RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet, Reference (..))
import           Data.Text                                     (Text)

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

entry :: Parser (Text, RawSelection)
entry = do
  skipSpace
  index <- getPosition
  key <- token
  args <- try arguments <|> pure []
  value <-
    try (body args) <|>
    pure
      (RawSelectionField $
       RawSelection' {rawSelectionArguments = args, rawSelectionRec = (), rawSelectionPosition = index})
  return (key, value)

{--
  {
    bigImage: image(320)
    smallImage:  image(640)
  }
--}
alias :: Parser (Text, RawSelection)
alias = do
  (name_, position_) <- qualifier
  _ <- char ':'
  value' <- entry
  return value'

separated :: Parser a -> Parser [a]
separated x = x `sepBy` separator

entries :: Parser RawSelectionSet
entries = do
  _ <- char '{'
  skipSpace
  entries' <- separated (alias <|> inlineFragment <|> spread <|> entry)
  skipSpace
  _ <- char '}'
  return entries'

body :: RawArguments -> Parser RawSelection
body args = do
  skipSpace
  index <- getPosition
  entries' <- entries
  return
    (RawSelectionSet $
     RawSelection' {rawSelectionArguments = args, rawSelectionRec = entries', rawSelectionPosition = index})
