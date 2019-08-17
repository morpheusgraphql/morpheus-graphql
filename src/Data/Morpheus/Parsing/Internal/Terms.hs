{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( onType
  , spaceAndComments
  , spreadLiteral
  , parseNonNull
  , parseMaybeTuple
  , parseAssignment
  ) where

import           Data.Functor                             (($>))
import           Data.Morpheus.Parsing.Internal.Internal  (Parser, Position)
import           Data.Morpheus.Parsing.Internal.Primitive (token)
import           Data.Morpheus.Types.Internal.Data        (DataTypeWrapper (..))
import           Data.Text                                (Text)
import           Text.Megaparsec                          (between, getSourcePos, label, sepBy, skipMany, skipManyTill,
                                                           (<?>), (<|>))
import           Text.Megaparsec.Char                     (char, newline, printChar, space, space1, string)

spaceAndComments :: Parser ()
spaceAndComments = space *> skipMany inlineComment *> space
  where
    inlineComment = char '#' *> skipManyTill printChar newline *> space

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  spaceAndComments
  return wrapper

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
  between (char '(' *> space) (char ')' *> space) (parser `sepBy` (char ',' *> space) <?> "empty Tuple value!")

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' =
  label "assignment" $ do
    name' <- nameParser'
    char ':' *> space
    value' <- valueParser'
    pure (name', value')

onType :: Parser Text
onType = do
  _ <- string "on"
  space1
  token

spreadLiteral :: Parser Position
spreadLiteral = do
  index <- getSourcePos
  _ <- string "..."
  space
  return index
