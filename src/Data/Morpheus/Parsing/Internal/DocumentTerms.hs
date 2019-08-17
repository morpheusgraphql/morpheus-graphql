{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Internal.DocumentTerms
  ( parseMaybeTuple
  , parseAssignment
  , token
  , qualifier
  , pipe
  , setOf
  , Parser
  , Position
  ) where

import           Data.Morpheus.Parsing.Internal.Terms (spaceAndComments)
import           Data.Morpheus.Types.Internal.Value   (convertToHaskellName)
import           Data.Text                            (Text)
import qualified Data.Text                            as T (pack)
import           Data.Void                            (Void)
import           Text.Megaparsec                      (Parsec, SourcePos, between, getSourcePos, label, many, sepBy,
                                                       sepEndBy, (<?>), (<|>))
import           Text.Megaparsec.Char                 (char, digitChar, letterChar)

type Position = SourcePos

type Parser = Parsec Void Text

setOf :: Parser a -> Parser [a]
setOf entry = setLiteral (entry `sepEndBy` many (char ',' *> spaceAndComments))

setLiteral :: Parser [a] -> Parser [a]
setLiteral = between (char '{' *> spaceAndComments) (char '}' *> spaceAndComments)

pipe :: Parser ()
pipe = char '|' *> spaceAndComments

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
  between
    (char '(' *> spaceAndComments)
    (char ')' *> spaceAndComments)
    (parser `sepBy` (many (char ',') *> spaceAndComments) <?> "empty Tuple value!")

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' =
  label "assignment" $ do
    name' <- nameParser'
    char ':' *> spaceAndComments
    value' <- valueParser'
    pure (name', value')

token :: Parser Text
token =
  label "token" $ do
    firstChar <- letterChar <|> char '_'
    restToken <- many $ letterChar <|> char '_' <|> digitChar
    spaceAndComments
    return $ convertToHaskellName $ T.pack $ firstChar : restToken

qualifier :: Parser (Text, Position)
qualifier =
  label "qualifier" $ do
    position' <- getSourcePos
    value <- token
    return (value, position')
