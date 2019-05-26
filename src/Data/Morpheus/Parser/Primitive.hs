{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import           Control.Applicative           (many, (<|>))
import           Data.Attoparsec.Text          (Parser, char, digit, letter, skipSpace)
import           Data.Morpheus.Parser.Internal (getPosition)
import           Data.Text                     (Text)
import qualified Data.Text                     as T (pack)

replaceType :: Text -> Text
replaceType "type" = "_type"
replaceType x      = x

token :: Parser Text
token = do
  firstChar <- letter <|> char '_'
  restToken <- many $ letter <|> char '_' <|> digit
  return $ replaceType $ T.pack $ firstChar : restToken

qualifier :: Parser (Text, Int)
qualifier = do
  skipSpace
  position' <- getPosition
  value <- token
  return (value, position')

variable :: Parser (Text, Int)
variable = do
  skipSpace
  position' <- getPosition
  _ <- char '$'
  varName' <- token
  return (varName', position')

separator :: Parser Char
separator = char ',' <|> char ' ' <|> char '\n' <|> char '\t'
