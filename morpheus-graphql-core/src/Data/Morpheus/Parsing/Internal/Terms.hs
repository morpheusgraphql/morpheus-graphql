{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
    toLBS,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Literals (Lit (..))
import qualified Data.Morpheus.Types.Internal.AST as AST
import Data.Morpheus.Types.Internal.AST
  ( DataTypeWrapper (..),
    Description,
    FieldName (..),
    Ref (..),
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

-- parens : '()'
parens :: Parser a -> Parser a
parens = between (symbol 40) (symbol 41)
{-# INLINEABLE parens #-}

-- braces: {}
braces :: Parser a -> Parser a
braces = between (symbol 123) (symbol 125)
{-# INLINEABLE braces #-}

-- comma: ,
comma :: Parser ()
comma = char (lit (Proxy @",")) *> space
{-# INLINEABLE comma #-}

-- brackets: []
brackets :: Parser a -> Parser a
brackets = between (symbol 91) (symbol 93)
{-# INLINEABLE brackets #-}

-- underscore : '_'
underscore :: Parser Word8
underscore = char (lit (Proxy @"_"))
{-# INLINEABLE underscore #-}

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
  where
    comment =
      label "Comment" $
        char 35 *> skipManyTill printChar newline *> space
    {-# INLINEABLE comment #-}
{-# INLINEABLE ignoredTokens #-}

ignoredTokens1 :: Parser ()
ignoredTokens1 = space1 *> ignoredTokens
{-# INLINEABLE ignoredTokens1 #-}

-- 2.1.9 Names
-- https://spec.graphql.org/draft/#Name
-- Name ::
--  NameStart NameContinue[list,opt]
--
name :: Parser AST.Token
name =
  label "Name" $
    fromLBS . pack
      <$> ((:) <$> nameStartBS <*> nameContinueBS)
      <* ignoredTokens
{-# INLINEABLE name #-}

escapedChar :: Parser Char
escapedChar = label "EscapedChar" $ printChar >>= handleEscape
{-# INLINEABLE escapedChar #-}

str :: ByteString -> Parser ()
str x = string x $> ()
{-# INLINEABLE str #-}

-- exclamationMark: '!'
exclamationMark :: Parser ()
exclamationMark = symbol $ lit $ Proxy @"!"
{-# INLINEABLE exclamationMark #-}

-- dollar :: $
dollar :: Parser ()
dollar = symbol $ lit $ Proxy @"$"
{-# INLINEABLE dollar #-}

-- at: '@'
at :: Parser ()
at = symbol $ lit $ Proxy @"@"
{-# INLINEABLE at #-}

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

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens
{-# INLINEABLE symbol #-}

-- NameStart::
--   Letter
--   _
nameStartBS :: Parser Word8
nameStartBS = letterChar <|> underscore
{-# INLINEABLE nameStartBS #-}

--  NameContinue::
--   Letter
--   Digit
nameContinueBS :: Parser [Word8]
nameContinueBS = many (letterChar <|> underscore <|> digitChar)
{-# INLINEABLE nameContinueBS #-}

parseNegativeSign :: Parser Bool
parseNegativeSign = (minus $> True <* ignoredTokens) <|> pure False
{-# INLINEABLE parseNegativeSign #-}

parseName :: Parser FieldName
parseName = FieldName <$> name
{-# INLINEABLE parseName #-}

parseTypeName :: Parser TypeName
parseTypeName = label "TypeName" $ TypeName <$> name
{-# INLINEABLE parseTypeName #-}

keyword :: FieldName -> Parser ()
keyword (FieldName x) = string (toLBS x) *> ignoredTokens1
{-# INLINEABLE keyword #-}

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

parseString :: Parser AST.Token
parseString = blockString <|> inlineString
{-# INLINEABLE parseString #-}

blockString :: Parser AST.Token
blockString = stringWith (str "\"\"\"") (w2c <$> (printChar <|> newline))
{-# INLINEABLE blockString #-}

inlineString :: Parser AST.Token
inlineString = stringWith (str "\"") escapedChar
{-# INLINEABLE inlineString #-}

stringWith :: Parser quote -> Parser Char -> Parser AST.Token
stringWith quote parser =
  T.pack
    <$> ( quote
            *> manyTill parser quote
            <* ignoredTokens
        )
{-# INLINEABLE stringWith #-}

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

optionalCollection :: (Empty c) => Parser c -> Parser c
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
spreadLiteral = getLocation <* str "..." <* ignoredTokens
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
