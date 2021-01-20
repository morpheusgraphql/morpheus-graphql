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
    Stream,
    Tokens,
    between,
    label,
    many,
    manyTill,
    sepBy,
    sepBy1,
    sepEndBy,
    skipManyTill,
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

parseNegativeSign :: Stream s => Parser s Bool
parseNegativeSign = (minus $> True <* ignoredTokens) <|> pure False
{-# INLINEABLE parseNegativeSign #-}

parseName :: Stream s => Parser s FieldName
parseName = FieldName <$> name
{-# INLINEABLE parseName #-}

parseTypeName :: Stream s => Parser s TypeName
parseTypeName = label "TypeName" $ TypeName <$> name
{-# INLINEABLE parseTypeName #-}

keyword :: (Stream s) => ByteString -> Parser s ()
keyword word = string word *> space1 *> ignoredTokens
{-# INLINEABLE keyword #-}

symbol :: Stream s => Word8 -> Parser s ()
symbol x = char x *> ignoredTokens
{-# INLINEABLE symbol #-}

-- braces: {}
braces :: Stream s => Parser s a -> Parser s a
braces = between (symbol 123) (symbol 125)
{-# INLINEABLE braces #-}

-- brackets: []
brackets :: Stream s => Parser s a -> Parser s a
brackets = between (symbol 91) (symbol 93)
{-# INLINEABLE brackets #-}

-- parens : '()'
parens :: Stream s => Parser s a -> Parser s a
parens = between (symbol 40) (symbol 41)
{-# INLINEABLE parens #-}

-- underscore : '_'
underscore :: Stream s => Parser s Word8
underscore = char 95
{-# INLINEABLE underscore #-}

comma :: Stream s => Parser s ()
comma = label "," $ char 44 *> space
{-# INLINEABLE comma #-}

-- dollar :: $
dollar :: Stream s => Parser s ()
dollar = label "$" $ symbol 36
{-# INLINEABLE dollar #-}

-- equal :: '='
equal :: Stream s => Parser s ()
equal = label "=" $ symbol 61
{-# INLINEABLE equal #-}

-- colon :: ':'
colon :: Stream s => Parser s ()
colon = label ":" $ symbol 58
{-# INLINEABLE colon #-}

-- minus: '-'
minus :: Stream s => Parser s ()
minus = label "-" $ symbol 45
{-# INLINEABLE minus #-}

-- verticalPipe: '|'
verticalPipe :: Stream s => Parser s ()
verticalPipe = label "|" $ symbol 124
{-# INLINEABLE verticalPipe #-}

ampersand :: Stream s => Parser s ()
ampersand = label "&" $ symbol 38
{-# INLINEABLE ampersand #-}

-- at: '@'
at :: Stream s => Parser s ()
at = label "@" $ symbol 64
{-# INLINEABLE at #-}

-- PRIMITIVE
------------------------------------

-- 2.1.9 Names
-- https://spec.graphql.org/draft/#Name
-- Name ::
--  NameStart NameContinue[list,opt]
--
name :: Stream s => Parser s Token
name =
  label "Name" $
    fromLBS . pack
      <$> ((:) <$> nameStart <*> nameContinue)
      <* ignoredTokens
{-# INLINEABLE name #-}

-- NameStart::
--   Letter
--   _
nameStart :: Stream s => Parser s Word8
nameStart = letterChar <|> underscore
{-# INLINEABLE nameStart #-}

--  NameContinue::
--   Letter
--   Digit
nameContinue :: Stream s => Parser s [Word8]
nameContinue = many (letterChar <|> underscore <|> digitChar)
{-# INLINEABLE nameContinue #-}

varName :: Stream s => Parser s FieldName
varName = dollar *> parseName <* ignoredTokens
{-# INLINEABLE varName #-}

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Stream s => Parser s (Ref FieldName)
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
optDescription :: Stream s => Parser s (Maybe Description)
optDescription = optional parseString
{-# INLINEABLE optDescription #-}

parseString :: Stream s => Parser s Token
parseString = blockString <|> singleLineString
{-# INLINEABLE parseString #-}

blockString :: Stream s => Parser s Token
blockString = stringWith (string "\"\"\"") (w2c <$> (printChar <|> newline))
{-# INLINEABLE blockString #-}

singleLineString :: Stream s => Parser s Token
singleLineString = stringWith (string "\"") escapedChar
{-# INLINEABLE singleLineString #-}

stringWith :: Stream s => Parser s quote -> Parser s Char -> Parser s Token
stringWith quote parser =
  T.pack
    <$> ( quote
            *> manyTill parser quote
            <* ignoredTokens
        )
{-# INLINEABLE stringWith #-}

escapedChar :: Stream s => Parser s Char
escapedChar = label "EscapedChar" $ printChar >>= handleEscape
{-# INLINEABLE escapedChar #-}

handleEscape :: Stream s => Word8 -> Parser s Char
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
ignoredTokens :: Stream s => Parser s ()
ignoredTokens =
  label "IgnoredTokens" $
    space
      *> many (comment <|> comma)
      *> space
{-# INLINEABLE ignoredTokens #-}

comment :: Stream s => Parser s ()
comment =
  label "Comment" $
    char 35 *> skipManyTill printChar newline *> space
{-# INLINEABLE comment #-}

-- exclamationMark: '!'
exclamationMark :: Stream s => Parser s ()
exclamationMark = label "!" $symbol 33
{-# INLINEABLE exclamationMark #-}

------------------------------------------------------------------------
sepByAnd :: Stream s => Parser s a -> Parser s [a]
sepByAnd entry = entry `sepBy` (optional ampersand *> ignoredTokens)
{-# INLINEABLE sepByAnd #-}

pipe :: Stream s => Parser s a -> Parser s [a]
pipe x = optional verticalPipe *> (x `sepBy1` verticalPipe)
{-# INLINEABLE pipe #-}

-----------------------------
collection :: Stream s => Parser s a -> Parser s [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)
{-# INLINEABLE collection #-}

setOf :: (FromElems Eventless a coll, KeyOf k a, Stream s) => Parser s a -> Parser s coll
setOf = collection >=> lift . fromElems
{-# INLINEABLE setOf #-}

optionalCollection :: (Empty c, Stream s) => Parser s c -> Parser s c
optionalCollection x = x <|> pure empty
{-# INLINEABLE optionalCollection #-}

parseNonNull :: Stream s => Parser s [DataTypeWrapper]
parseNonNull =
  (exclamationMark $> [NonNullType])
    <|> pure []
{-# INLINEABLE parseNonNull #-}

uniqTuple :: (FromElems Eventless a coll, Stream s, KeyOf k a) => Parser s a -> Parser s coll
uniqTuple parser =
  label "Tuple" $
    parens
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= lift . fromElems
{-# INLINEABLE uniqTuple #-}

uniqTupleOpt :: (FromElems Eventless a coll, Stream s, Empty coll, KeyOf k a) => Parser s a -> Parser s coll
uniqTupleOpt x = uniqTuple x <|> pure empty
{-# INLINEABLE uniqTupleOpt #-}

fieldNameColon :: Stream s => Parser s FieldName
fieldNameColon = parseName <* colon
{-# INLINEABLE fieldNameColon #-}

-- Type Conditions: https://graphql.github.io/graphql-spec/June2018/#sec-Type-Conditions
--
--  TypeCondition:
--    on NamedType
--
parseTypeCondition :: Stream s => Parser s TypeName
parseTypeCondition = keyword "on" *> parseTypeName
{-# INLINEABLE parseTypeCondition #-}

spreadLiteral :: Stream s => Parser s Position
spreadLiteral = getLocation <* string "..." <* space
{-# INLINEABLE spreadLiteral #-}

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: Stream s => Parser s (Maybe FieldName)
parseAlias = try (optional alias) <|> pure Nothing
  where
    alias = label "alias" fieldNameColon
{-# INLINEABLE parseAlias #-}

parseType :: Stream s => Parser s TypeRef
parseType = parseTypeW <$> parseWrappedType <*> parseNonNull
{-# INLINEABLE parseType #-}

parseTypeW :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> TypeRef
parseTypeW (wrappers, typeConName) nonNull =
  TypeRef
    { typeConName,
      typeWrappers = toHSWrappers (nonNull <> wrappers)
    }
{-# INLINEABLE parseTypeW #-}

parseWrappedType :: Stream s => Parser s ([DataTypeWrapper], TypeName)
parseWrappedType = (unwrapped <|> wrapped) <* ignoredTokens
  where
    unwrapped :: Stream s => Parser s ([DataTypeWrapper], TypeName)
    unwrapped = ([],) <$> parseTypeName <* ignoredTokens
    ----------------------------------------------
    wrapped :: Stream s => Parser s ([DataTypeWrapper], TypeName)
    wrapped = brackets (wrapAsList <$> (unwrapped <|> wrapped) <*> parseNonNull)
{-# INLINEABLE parseWrappedType #-}

wrapAsList :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> ([DataTypeWrapper], TypeName)
wrapAsList (wrappers, tName) nonNull = (ListType : nonNull <> wrappers, tName)
{-# INLINEABLE wrapAsList #-}
