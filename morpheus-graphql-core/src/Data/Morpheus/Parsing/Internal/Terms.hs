{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( name,
    variable,
    varName,
    ignoredTokens,
    parseString,
    collection,
    setOf,
    uniqTuple,
    uniqTupleOpt,
    parseTypeCondition,
    spreadLiteral,
    parseNonNull,
    parseWrappedType,
    parseAlias,
    sepByAnd,
    parseName,
    parseType,
    keyword,
    symbol,
    optDescription,
    optionalCollection,
    parseNegativeSign,
    parseTypeName,
    pipe,
    fieldNameColon,
    brackets,
    equal,
    comma,
    colon,
    at,
  )
where

import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy
  ( pack,
  )
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Morpheus.Ext.Result (Eventless)
import Data.Morpheus.Internal.Utils
  ( Empty (..),
    FromElems (..),
    KeyOf,
    fromElems,
    fromLBS,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataTypeWrapper (..),
    Description,
    FieldName (..),
    Ref (..),
    Token,
    TypeName (..),
    TypeRef (..),
    toHSWrappers,
  )
import qualified Data.Text as T
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( (<?>),
    between,
    label,
    many,
    manyTill,
    sepBy,
    sepBy1,
    sepEndBy,
    try,
  )
import Text.Megaparsec.Byte
  ( char,
    digitChar,
    letterChar,
    newline,
    printChar,
    space,
    space1,
    string,
  )
import Text.Megaparsec.Byte.Lexer (skipLineComment)

parseNegativeSign :: Parser Bool
parseNegativeSign = (minus $> True <* ignoredTokens) <|> pure False
{-# INLINEABLE parseNegativeSign #-}

parseName :: Parser FieldName
parseName = FieldName <$> name
{-# INLINEABLE parseName #-}

parseTypeName :: Parser TypeName
parseTypeName = label "TypeName" $ TypeName <$> name
{-# INLINEABLE parseTypeName #-}

keyword :: ByteString -> Parser ()
keyword word = string word *> space1 *> ignoredTokens
{-# INLINEABLE keyword #-}

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens
{-# INLINEABLE symbol #-}

-- braces: {}
braces :: Parser a -> Parser a
braces = between (symbol 123) (symbol 125)
{-# INLINEABLE braces #-}

-- brackets: []
brackets :: Parser a -> Parser a
brackets = between (symbol 91) (symbol 93)
{-# INLINEABLE brackets #-}

-- parens : '()'
parens :: Parser a -> Parser a
parens = between (symbol 40) (symbol 41)
{-# INLINEABLE parens #-}

-- underscore : '_'
underscore :: Parser Word8
underscore = char 95
{-# INLINEABLE underscore #-}

comma :: Parser ()
comma = label "," $ char 44 *> space
{-# INLINEABLE comma #-}

-- dollar :: $
dollar :: Parser ()
dollar = label "$" $ symbol 36
{-# INLINEABLE dollar #-}

-- equal :: '='
equal :: Parser ()
equal = label "=" $ symbol 61
{-# INLINEABLE equal #-}

-- colon :: ':'
colon :: Parser ()
colon = label ":" $ symbol 58
{-# INLINEABLE colon #-}

-- minus: '-'
minus :: Parser ()
minus = label "-" $ symbol 45
{-# INLINEABLE minus #-}

-- verticalPipe: '|'
verticalPipe :: Parser ()
verticalPipe = label "|" $ symbol 124
{-# INLINEABLE verticalPipe #-}

ampersand :: Parser ()
ampersand = label "&" $ symbol 38
{-# INLINEABLE ampersand #-}

-- at: '@'
at :: Parser ()
at = label "@" $ symbol 64
{-# INLINEABLE at #-}

-- PRIMITIVE
------------------------------------

-- 2.1.9 Names
-- https://spec.graphql.org/draft/#Name
-- Name ::
--  NameStart NameContinue[list,opt]
--
name :: Parser Token
name =
  label "Name" $
    fromLBS . pack
      <$> ((:) <$> nameStart <*> nameContinue)
      <* ignoredTokens
{-# INLINEABLE name #-}

-- NameStart::
--   Letter
--   _
nameStart :: Parser Word8
nameStart = letterChar <|> underscore
{-# INLINEABLE nameStart #-}

--  NameContinue::
--   Letter
--   Digit
nameContinue :: Parser [Word8]
nameContinue = many (letterChar <|> underscore <|> digitChar)
{-# INLINEABLE nameContinue #-}

varName :: Parser FieldName
varName = dollar *> parseName <* ignoredTokens
{-# INLINEABLE varName #-}

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Parser (Ref FieldName)
variable =
  label "variable" $
    flip Ref
      <$> getLocation
      <*> varName
{-# INLINEABLE variable #-}

-- Descriptions: https://graphql.github.io/graphql-spec/June2018/#Description
--
-- Description:
--   StringValue
optDescription :: Parser (Maybe Description)
optDescription = optional parseString
{-# INLINEABLE optDescription #-}

parseString :: Parser Token
parseString = blockString <|> singleLineString
{-# INLINEABLE parseString #-}

blockString :: Parser Token
blockString = stringWith (string "\"\"\"") (w2c <$> (printChar <|> newline))
{-# INLINEABLE blockString #-}

singleLineString :: Parser Token
singleLineString = stringWith (string "\"") escapedChar
{-# INLINEABLE singleLineString #-}

stringWith :: Parser quote -> Parser Char -> Parser Token
stringWith quote parser =
  T.pack
    <$> ( quote
            *> manyTill parser quote
            <* ignoredTokens
        )
{-# INLINEABLE stringWith #-}

escapedChar :: Parser Char
escapedChar = label "EscapedChar" $ printChar >>= handleEscape
{-# INLINEABLE escapedChar #-}

handleEscape :: Word8 -> Parser Char
handleEscape 92 = w2c . escape <$> printChar
handleEscape x = pure (w2c x)
{-# INLINEABLE handleEscape #-}

escape :: Word8 -> Word8
escape 98 = 8
escape 110 = 10
escape 102 = 12
escape 114 = 13
escape 116 = 9
escape x = x
{-# INLINEABLE escape #-}

-- Ignored Tokens : https://graphql.github.io/graphql-spec/June2018/#sec-Source-Text.Ignored-Tokens
--  Ignored:
--    UnicodeBOM
--    WhiteSpace
--    LineTerminator
--    Comment
--    Comma
ignoredTokens :: Parser ()
ignoredTokens =
  label "IgnoredTokens" $
    space
      *> many (comment <|> comma)
      *> space
{-# INLINEABLE ignoredTokens #-}

comment :: Parser ()
comment = label "Comment" $ skipLineComment "#"
{-# INLINEABLE comment #-}

-- exclamationMark: '!'
exclamationMark :: Parser ()
exclamationMark = label "!" $symbol 33
{-# INLINEABLE exclamationMark #-}

------------------------------------------------------------------------
sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional ampersand *> ignoredTokens)
{-# INLINEABLE sepByAnd #-}

pipe :: Parser a -> Parser [a]
pipe x = optional verticalPipe *> (x `sepBy1` verticalPipe)
{-# INLINEABLE pipe #-}

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)
{-# INLINEABLE collection #-}

setOf :: (FromElems Eventless a coll, KeyOf k a) => Parser a -> Parser coll
setOf = collection >=> lift . fromElems
{-# INLINEABLE setOf #-}

optionalCollection :: Empty c => Parser c -> Parser c
optionalCollection x = x <|> pure empty
{-# INLINEABLE optionalCollection #-}

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull =
  (exclamationMark $> [NonNullType])
    <|> pure []
{-# INLINEABLE parseNonNull #-}

uniqTuple :: (FromElems Eventless a coll, KeyOf k a) => Parser a -> Parser coll
uniqTuple parser =
  label "Tuple" $
    parens
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= lift . fromElems
{-# INLINEABLE uniqTuple #-}

uniqTupleOpt :: (FromElems Eventless a coll, Empty coll, KeyOf k a) => Parser a -> Parser coll
uniqTupleOpt x = uniqTuple x <|> pure empty
{-# INLINEABLE uniqTupleOpt #-}

fieldNameColon :: Parser FieldName
fieldNameColon = parseName <* colon
{-# INLINEABLE fieldNameColon #-}

-- Type Conditions: https://graphql.github.io/graphql-spec/June2018/#sec-Type-Conditions
--
--  TypeCondition:
--    on NamedType
--
parseTypeCondition :: Parser TypeName
parseTypeCondition = keyword "on" *> parseTypeName
{-# INLINEABLE parseTypeCondition #-}

spreadLiteral :: Parser Position
spreadLiteral = getLocation <* string "..." <* space
{-# INLINEABLE spreadLiteral #-}

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: Parser (Maybe FieldName)
parseAlias = try (optional alias) <|> pure Nothing
  where
    alias = label "alias" fieldNameColon
{-# INLINEABLE parseAlias #-}

parseType :: Parser TypeRef
parseType = parseTypeW <$> parseWrappedType <*> parseNonNull
{-# INLINEABLE parseType #-}

parseTypeW :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> TypeRef
parseTypeW (wrappers, typeConName) nonNull =
  TypeRef
    { typeConName,
      typeWrappers = toHSWrappers (nonNull <> wrappers)
    }
{-# INLINEABLE parseTypeW #-}

parseWrappedType :: Parser ([DataTypeWrapper], TypeName)
parseWrappedType = (unwrapped <|> wrapped) <* ignoredTokens
  where
    unwrapped :: Parser ([DataTypeWrapper], TypeName)
    unwrapped = ([],) <$> parseTypeName <* ignoredTokens
    ----------------------------------------------
    wrapped :: Parser ([DataTypeWrapper], TypeName)
    wrapped = brackets (wrapAsList <$> (unwrapped <|> wrapped) <*> parseNonNull)
{-# INLINEABLE parseWrappedType #-}

wrapAsList :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> ([DataTypeWrapper], TypeName)
wrapAsList (wrappers, tName) nonNull = (ListType : nonNull <> wrappers, tName)
{-# INLINEABLE wrapAsList #-}
