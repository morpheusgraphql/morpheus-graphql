{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Primitive where

import qualified Data.Text                     as T
                                                ( Text
                                                , pack
                                                , concat
                                                )
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text          as AT
                                                ( takeWhile )
import           Control.Applicative
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Control.Applicative
import           Data.Char


replaceType :: T.Text -> T.Text
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



--escape :: Parser String
--escape = do
--    d <- char '\\'
--    c <- inClass "\\\"0nrvtbf"
--    return [d, c]

-- nonEscape :: Parser Char
-- nonEscape = notInClass "\\\"\0\n\r\v\t\b\f"

-- character :: Parser String
-- character = fmap return nonEscape <|> escape

-- parseString :: Parser String
-- parseString = do
--     char '"'
--     strings <- many character
--     char '"'
--     return $ concat strings

escaped = char '\\' >> choice (zipWith escapedChar codes replacements)

escapedChar code replacement = char code >> return replacement

codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']


jsString :: Parser JSType
jsString = do
    char '"'
    value <- many (escaped <|> letter)
    char '"'
    pure $ JSString $ T.pack value

token :: Parser T.Text
token = replaceType . T.pack <$> some (letter <|> char '_')

variable :: Parser T.Text
variable = skipSpace *> char '$' *> token

separator :: Parser Char
separator = char ',' <|> char ' ' <|> char '\n' <|> char '\t'
