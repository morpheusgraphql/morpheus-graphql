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
    Term,
    number,
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
import qualified Data.Morpheus.Types.Internal.AST as AST
import Data.Morpheus.Types.Internal.AST
  ( DataTypeWrapper (..),
    Description,
    FieldName (..),
    Ref (..),
    -- Token,
    TypeName (..),
    TypeRef (..),
    toHSWrappers,
  )
import Data.Scientific
  ( Scientific,
  )
import qualified Data.Text as T
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( (<?>),
    Stream,
    Token,
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
import Text.Megaparsec.Byte.Lexer (scientific)
import qualified Text.Megaparsec.Char as T
import qualified Text.Megaparsec.Char.Lexer as T (scientific)

class Term s where
  -- parens : '()'
  parens :: Parser s a -> Parser s a

  -- braces: {}
  braces :: Parser s a -> Parser s a

  -- comma: ,
  comma :: Parser s ()

  -- brackets: []
  brackets :: Parser s a -> Parser s a

  -- underscore : '_'
  underscore :: Parser s (Token s)

  -- Ignored Tokens : https://graphql.github.io/graphql-spec/June2018/#sec-Source-Text.Ignored-Tokens
  --  Ignored:
  --    UnicodeBOM
  --    WhiteSpace
  --    LineTerminator
  --    Comment
  --    Comma
  ignoredTokens :: Parser s ()

  ignoredTokens1 :: Parser s ()

  -- PRIMITIVE
  ------------------------------------

  -- 2.1.9 Names
  -- https://spec.graphql.org/draft/#Name
  -- Name ::
  --  NameStart NameContinue[list,opt]
  --
  name :: Parser s AST.Token

  anyChar :: Parser s Char

  anyChar' :: Parser s (Token s)

  escapedChar :: Parser s Char

  str :: s -> Parser s ()

  nline :: Parser s Char

  -- exclamationMark: '!'
  exclamationMark :: Parser s ()

  -- dollar :: $
  dollar :: Parser s ()

  -- at: '@'
  at :: Parser s ()

  -- equal :: '='
  equal :: Parser s ()

  -- colon :: ':'
  colon :: Parser s ()

  -- minus: '-'
  minus :: Parser s ()

  -- verticalPipe: '|'
  verticalPipe :: Parser s ()

  ampersand :: Parser s ()

  number :: Parser s Scientific

  symbol :: Token s -> Parser s ()

-- NameStart::
--   Letter
--   _
nameStartBS :: Parser ByteString Word8
nameStartBS = letterChar <|> underscore
{-# INLINEABLE nameStartBS #-}

--  NameContinue::
--   Letter
--   Digit
nameContinueBS :: Parser ByteString [Word8]
nameContinueBS = many (letterChar <|> underscore <|> digitChar)
{-# INLINEABLE nameContinueBS #-}

nameStartT :: Parser Text Char
nameStartT = T.letterChar <|> underscore
{-# INLINEABLE nameStartT #-}

--  NameContinue::
--   Letter
--   Digit
nameContinueT :: Parser Text String
nameContinueT = many (T.letterChar <|> underscore <|> T.digitChar)
{-# INLINEABLE nameContinueT #-}

instance Term ByteString where
  parens = between (symbol 40) (symbol 41)
  {-# INLINEABLE parens #-}

  braces = between (symbol 123) (symbol 125)
  {-# INLINEABLE braces #-}

  comma = label "," $ char 44 *> space
  {-# INLINEABLE comma #-}

  brackets = between (symbol 91) (symbol 93)
  {-# INLINEABLE brackets #-}

  underscore = char 95
  {-# INLINEABLE underscore #-}

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

  ignoredTokens1 = space1 *> ignoredTokens
  {-# INLINEABLE ignoredTokens1 #-}

  name =
    label "Name" $
      fromLBS . pack
        <$> ((:) <$> nameStartBS <*> nameContinueBS)
        <* ignoredTokens
  {-# INLINEABLE name #-}

  escapedChar = label "EscapedChar" $ printChar >>= handleEscape
  {-# INLINEABLE escapedChar #-}

  anyChar = w2c <$> printChar

  anyChar' = printChar

  str x = string x $> ()
  {-# INLINEABLE str #-}

  nline = w2c <$> newline
  {-# INLINEABLE nline #-}

  exclamationMark = label "!" $ symbol 33
  {-# INLINEABLE exclamationMark #-}

  dollar = label "$" $ symbol 36
  {-# INLINEABLE dollar #-}

  at = label "@" $ symbol 64
  {-# INLINEABLE at #-}

  equal = label "=" $ symbol 61
  {-# INLINEABLE equal #-}

  colon = label ":" $ symbol 58
  {-# INLINEABLE colon #-}

  minus = label "-" $ symbol 45
  {-# INLINEABLE minus #-}

  verticalPipe = label "|" $ symbol 124
  {-# INLINEABLE verticalPipe #-}

  ampersand = label "&" $ symbol 38
  {-# INLINEABLE ampersand #-}

  number = scientific
  {-# INLINEABLE number #-}

  symbol x = char x *> ignoredTokens
  {-# INLINEABLE symbol #-}

instance Term Text where
  parens = between (symbol '(') (symbol ')')
  {-# INLINEABLE parens #-}

  braces = between (symbol '{') (symbol '}')
  {-# INLINEABLE braces #-}

  comma = T.char ',' *> T.space
  {-# INLINEABLE comma #-}

  brackets = between (symbol '[') (symbol ']')
  {-# INLINEABLE brackets #-}

  underscore = T.char '_'
  {-# INLINEABLE underscore #-}

  ignoredTokens =
    label "IgnoredTokens" $
      T.space
        *> many (comment <|> comma)
        *> T.space
    where
      comment =
        label "Comment" $
          T.char '#' *> skipManyTill T.printChar T.newline *> T.space
      {-# INLINEABLE comment #-}
  {-# INLINEABLE ignoredTokens #-}

  ignoredTokens1 = T.space1 *> ignoredTokens
  {-# INLINEABLE ignoredTokens1 #-}

  name =
    label "Name" $
      T.pack
        <$> ((:) <$> nameStartT <*> nameContinueT)
        <* ignoredTokens
  {-# INLINEABLE name #-}

  escapedChar = label "EscapedChar" $ anyChar >>= handleEscapeChar
  {-# INLINEABLE escapedChar #-}

  anyChar = T.printChar

  anyChar' = T.printChar

  str x = string x $> ()
  {-# INLINEABLE str #-}

  nline = T.newline
  {-# INLINEABLE nline #-}

  exclamationMark = symbol '!'
  {-# INLINEABLE exclamationMark #-}

  dollar = symbol '$'
  {-# INLINEABLE dollar #-}

  at = symbol '@'
  {-# INLINEABLE at #-}

  equal = symbol '='
  {-# INLINEABLE equal #-}

  colon = symbol ':'
  {-# INLINEABLE colon #-}

  minus = symbol '-'
  {-# INLINEABLE minus #-}

  verticalPipe = symbol '|'
  {-# INLINEABLE verticalPipe #-}

  ampersand = symbol '&'
  {-# INLINEABLE ampersand #-}

  number = T.scientific
  {-# INLINEABLE number #-}

  symbol x = T.char x *> ignoredTokens
  {-# INLINEABLE symbol #-}

parseNegativeSign :: (Stream s, Term s) => Parser s Bool
parseNegativeSign = (minus $> True <* ignoredTokens) <|> pure False
{-# INLINEABLE parseNegativeSign #-}

parseName :: Term s => Parser s FieldName
parseName = FieldName <$> name
{-# INLINEABLE parseName #-}

parseTypeName :: (Stream s, Term s) => Parser s TypeName
parseTypeName = label "TypeName" $ TypeName <$> name
{-# INLINEABLE parseTypeName #-}

keyword :: (Stream s, Term s) => s -> Parser s ()
keyword word = str word *> ignoredTokens1
{-# INLINEABLE keyword #-}

varName :: (Stream s, Term s) => Parser s FieldName
varName = dollar *> parseName <* ignoredTokens
{-# INLINEABLE varName #-}

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: (Stream s, Term s) => Parser s (Ref FieldName)
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
optDescription :: (Stream s, IsString s, Term s) => Parser s (Maybe Description)
optDescription = optional parseString
{-# INLINEABLE optDescription #-}

parseString :: (Stream s, IsString s, Term s) => Parser s AST.Token
parseString = blockString <|> inlineString
{-# INLINEABLE parseString #-}

blockString :: (Stream s, IsString s, Term s) => Parser s AST.Token
blockString = stringWith (str "\"\"\"") (anyChar <|> nline)
{-# INLINEABLE blockString #-}

inlineString :: (Term s, IsString s, Stream s) => Parser s AST.Token
inlineString = stringWith (str "\"") escapedChar
{-# INLINEABLE inlineString #-}

stringWith :: (Stream s, Term s) => Parser s quote -> Parser s Char -> Parser s AST.Token
stringWith quote parser =
  T.pack
    <$> ( quote
            *> manyTill parser quote
            <* ignoredTokens
        )
{-# INLINEABLE stringWith #-}

handleEscapeChar :: (Stream s, Term s) => Char -> Parser s Char
handleEscapeChar '\\' = escapeChar <$> anyChar
handleEscapeChar x = pure x
{-# INLINEABLE handleEscapeChar #-}

escapeChar :: Char -> Char
escapeChar 'b' = '\b'
escapeChar 'n' = '\n'
escapeChar 'f' = '\f'
escapeChar 'r' = '\r'
escapeChar 't' = '\t'
escapeChar x = x
{-# INLINEABLE escapeChar #-}

handleEscape :: Word8 -> Parser ByteString Char
handleEscape 92 = w2c . escape <$> anyChar'
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
sepByAnd :: (Stream s, Term s) => Parser s a -> Parser s [a]
sepByAnd entry = entry `sepBy` (optional ampersand *> ignoredTokens)
{-# INLINEABLE sepByAnd #-}

pipe :: (Stream s, Term s) => Parser s a -> Parser s [a]
pipe x = optional verticalPipe *> (x `sepBy1` verticalPipe)
{-# INLINEABLE pipe #-}

-----------------------------
collection :: (Stream s, Term s) => Parser s a -> Parser s [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)
{-# INLINEABLE collection #-}

setOf :: (FromElems Eventless a coll, Term s, KeyOf k a, Stream s) => Parser s a -> Parser s coll
setOf = collection >=> lift . fromElems
{-# INLINEABLE setOf #-}

optionalCollection :: (Empty c, Stream s) => Parser s c -> Parser s c
optionalCollection x = x <|> pure empty
{-# INLINEABLE optionalCollection #-}

parseNonNull :: (Stream s, Term s) => Parser s [DataTypeWrapper]
parseNonNull =
  (exclamationMark $> [NonNullType])
    <|> pure []
{-# INLINEABLE parseNonNull #-}

uniqTuple :: (FromElems Eventless a coll, Term s, Stream s, KeyOf k a) => Parser s a -> Parser s coll
uniqTuple parser =
  label "Tuple" $
    parens
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= lift . fromElems
{-# INLINEABLE uniqTuple #-}

uniqTupleOpt :: (FromElems Eventless a coll, Term s, Stream s, Empty coll, KeyOf k a) => Parser s a -> Parser s coll
uniqTupleOpt x = uniqTuple x <|> pure empty
{-# INLINEABLE uniqTupleOpt #-}

fieldNameColon :: (Stream s, Term s) => Parser s FieldName
fieldNameColon = parseName <* colon
{-# INLINEABLE fieldNameColon #-}

-- Type Conditions: https://graphql.github.io/graphql-spec/June2018/#sec-Type-Conditions
--
--  TypeCondition:
--    on NamedType
--
parseTypeCondition :: (Stream s, Term s, IsString s) => Parser s TypeName
parseTypeCondition = keyword "on" *> parseTypeName
{-# INLINEABLE parseTypeCondition #-}

spreadLiteral :: (Stream s, Term s, IsString s) => Parser s Position
spreadLiteral = getLocation <* str "..." <* ignoredTokens
{-# INLINEABLE spreadLiteral #-}

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: (Stream s, Term s) => Parser s (Maybe FieldName)
parseAlias = try (optional alias) <|> pure Nothing
  where
    alias = label "alias" fieldNameColon
{-# INLINEABLE parseAlias #-}

parseType :: (Stream s, Term s) => Parser s TypeRef
parseType = parseTypeW <$> parseWrappedType <*> parseNonNull
{-# INLINEABLE parseType #-}

parseTypeW :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> TypeRef
parseTypeW (wrappers, typeConName) nonNull =
  TypeRef
    { typeConName,
      typeWrappers = toHSWrappers (nonNull <> wrappers)
    }
{-# INLINEABLE parseTypeW #-}

parseWrappedType :: (Stream s, Term s) => Parser s ([DataTypeWrapper], TypeName)
parseWrappedType = (unwrapped <|> wrapped) <* ignoredTokens
  where
    unwrapped :: (Stream s, Term s) => Parser s ([DataTypeWrapper], TypeName)
    unwrapped = ([],) <$> parseTypeName <* ignoredTokens
    ----------------------------------------------
    wrapped :: (Stream s, Term s) => Parser s ([DataTypeWrapper], TypeName)
    wrapped = brackets (wrapAsList <$> (unwrapped <|> wrapped) <*> parseNonNull)
{-# INLINEABLE parseWrappedType #-}

wrapAsList :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> ([DataTypeWrapper], TypeName)
wrapAsList (wrappers, tName) nonNull = (ListType : nonNull <> wrappers, tName)
{-# INLINEABLE wrapAsList #-}
