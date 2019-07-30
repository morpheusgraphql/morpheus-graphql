{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Parsing.Terms
  ( nonNull
  , parseMaybeTuple
  , parseAssignment
  , token
  , qualifier
  , pipe
  , Parser
  , Position
  ) where

import           Data.Functor                       (($>))
import           Data.Morpheus.Types.Internal.Data  (DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Value (convertToHaskellName)
import           Data.Text                          (Text)
import qualified Data.Text                          as T (pack)
import           Data.Void                          (Void)
import           Text.Megaparsec                    (Parsec, SourcePos, between, getSourcePos, label, many, sepBy,
                                                     (<?>), (<|>))
import           Text.Megaparsec.Char               (char, digitChar, letterChar, space)

type Position = SourcePos

type Parser = Parsec Void Text

pipe :: Parser ()
pipe = char '|' *> space


nonNull :: Parser [DataTypeWrapper]
nonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  space
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

token :: Parser Text
token =
  label "token" $ do
    firstChar <- letterChar <|> char '_'
    restToken <- many $ letterChar <|> char '_' <|> digitChar
    space
    return $ convertToHaskellName $ T.pack $ firstChar : restToken

qualifier :: Parser (Text, Position)
qualifier =
  label "qualifier" $ do
    position' <- getSourcePos
    value <- token
    return (value, position')
