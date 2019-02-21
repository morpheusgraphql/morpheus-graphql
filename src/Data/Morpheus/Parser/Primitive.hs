{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Attoparsec.Text
import           Control.Applicative
import           Data.Morpheus.Types.Types     (  JSType(..)
                                                )

replaceType :: Text -> Text
replaceType "type" = "_type"
replaceType x      = x

boolTrue :: Parser JSType
boolTrue = string "true" *> pure (JSBool True)

boolFalse :: Parser JSType
boolFalse = string "false" *> pure (JSBool False)

jsBool :: Parser JSType
jsBool = boolTrue <|> boolFalse

jsInt :: Parser JSType
jsInt = JSInt <$> decimal

jsString :: Parser JSType
jsString = do
    char '"'
    value <- many (notChar '"')
    char '"'
    pure $ JSString $ pack value

token :: Parser Text
token = replaceType . pack <$> some (letter <|> char '_')

variable :: Parser Text
variable = skipSpace *> char '$' *> token

separator :: Parser Char
separator = char ',' <|> char ' ' <|> char '\n' <|> char '\t'
