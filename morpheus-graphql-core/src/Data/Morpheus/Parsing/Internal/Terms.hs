{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( name,
    variable,
    ignoredTokens,
    parseString,
    -------------
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
    assignedFieldName,
    brackets,
    equal,
    comma,
    colon,
    at,
  )
where

import Control.Monad ((>=>))
-- MORPHEUS
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy
  ( ByteString,
    pack,
    unpack,
  )
import Data.Functor (($>))
import Data.Morpheus.Internal.Utils
  ( Collection,
    KeyOf,
    Listable (..),
    empty,
    fromElems,
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
    fromLBS,
    toHSWrappers,
    toLBS,
  )
import Data.Text
  ( strip,
  )
import GHC.Word (Word8)
import Text.Megaparsec
  ( (<?>),
    (<|>),
    between,
    choice,
    label,
    many,
    manyTill,
    optional,
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

parseNegativeSign :: Parser Bool
parseNegativeSign = (minus $> True <* ignoredTokens) <|> pure False

parseName :: Parser FieldName
parseName = FieldName <$> name

parseTypeName :: Parser TypeName
parseTypeName = label "TypeName" $ TypeName <$> name

keyword :: FieldName -> Parser ()
keyword (FieldName word) = string (toLBS word) *> space1 *> ignoredTokens

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens

-- braces: {}
braces :: Parser a -> Parser a
braces =
  between
    (char 123 *> ignoredTokens)
    (char 125 *> ignoredTokens)

-- brackets: []
brackets :: Parser a -> Parser a
brackets =
  between
    (char 91 *> ignoredTokens)
    (char 93 *> ignoredTokens)

-- underscore : '_'
underscore :: Parser Word8
underscore = char 95

-- dollar :: $
dollar :: Parser Word8
dollar = char 36

-- equal :: '='
equal :: Parser Word8
equal = char 61

-- colon :: ':'
colon :: Parser ()
colon = symbol 58

minus :: Parser ()
minus = label "-" $ symbol 45

-- verticalPipe: '|'
verticalPipe :: Parser ()
verticalPipe = label "|" $ symbol 124

ampersand :: Parser ()
ampersand = label "&" $ symbol 38

-- at: '@'
at :: Parser ()
at = label "@" $ symbol 64

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

-- NameStart::
--   Letter
--   _
nameStart :: Parser Word8
nameStart = letterChar <|> underscore

--  NameContinue::
--   Letter
--   Digit
nameContinue :: Parser [Word8]
nameContinue = many (letterChar <|> underscore <|> digitChar)

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Parser Ref
variable =
  label "variable" $
    (flip Ref <$> getLocation <*> (dollar *> parseName))
      <* ignoredTokens

-- Descriptions: https://graphql.github.io/graphql-spec/June2018/#Description
--
-- Description:
--   StringValue
parseDescription :: Parser Description
parseDescription = strip <$> parseString

optDescription :: Parser (Maybe Description)
optDescription = optional parseDescription

parseString :: Parser Token
parseString = blockString <|> singleLineString

blockString :: Parser Token
blockString = stringWith (string "\"\"\"") (printChar <|> newline)

singleLineString :: Parser Token
singleLineString = stringWith (string "\"") escapedChar

stringWith :: Parser quote -> Parser Word8 -> Parser Token
stringWith quote parser =
  fromLBS . pack
    <$> ( quote
            *> manyTill parser quote
            <* ignoredTokens
        )

escapedChar :: Parser Word8
escapedChar = label "EscapedChar" $ printChar >>= handleEscape

handleEscape :: Word8 -> Parser Word8
--handleEscape 92 = choice escape
handleEscape x = pure x

-- escape :: [Parser Word8]
-- escape = map escapeCh escapeOptions
--   where
--     escapeCh :: (Char, Char) -> Parser Char
--     escapeCh (code, replacement) = char code $> replacement

-- escapeOptions :: [(Char, Char)]
-- escapeOptions =
--   [ (98, 8),
--     ('n', '\n'),
--     ('f', '\f'),
--     ('r', '\r'),
--     ('t', '\t'),
--     ('\\', '\\'),
--     ('\"', '\"'),
--     ('/', '/')
--   ]

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
      *> many ignored
      *> space

ignored :: Parser ()
ignored =
  label "Ignored" $
    comment
      <|> comma

comment :: Parser ()
comment = label "Comment" $ octothorpe *> skipManyTill printChar newline *> space

comma :: Parser ()
comma = label "," $ char 44 *> space

-- parens : '()'
parens :: Parser a -> Parser a
parens =
  between
    (char 40 *> ignoredTokens)
    (char 41 *> ignoredTokens)

-- exclamationMark: '!'
exclamationMark :: Parser ()
exclamationMark = symbol 33

-- octothorpe: '#'
octothorpe :: Parser ()
octothorpe = label "#" $ symbol 35

------------------------------------------------------------------------

sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional ampersand *> ignoredTokens)

pipe :: Parser a -> Parser [a]
pipe x = optional verticalPipe *> (x `sepBy1` verticalPipe)

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)

setOf :: (Listable a coll, KeyOf k a) => Parser a -> Parser coll
setOf = collection >=> lift . fromElems

optionalCollection :: Collection a c => Parser c -> Parser c
optionalCollection x = x <|> pure empty

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull =
  (exclamationMark $> [NonNullType])
    <|> pure []

uniqTuple :: (Listable a coll, KeyOf k a) => Parser a -> Parser coll
uniqTuple parser =
  label "Tuple" $
    parens
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= lift . fromElems

uniqTupleOpt :: (Listable a coll, Collection a coll, KeyOf k a) => Parser a -> Parser coll
uniqTupleOpt x = uniqTuple x <|> pure empty

assignedFieldName :: Parser FieldName
assignedFieldName = parseName <* colon

-- Type Conditions: https://graphql.github.io/graphql-spec/June2018/#sec-Type-Conditions
--
--  TypeCondition:
--    on NamedType
--
parseTypeCondition :: Parser TypeName
parseTypeCondition = keyword "on" *> parseTypeName

spreadLiteral :: Parser Position
spreadLiteral = getLocation <* string "..." <* space

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: Parser (Maybe FieldName)
parseAlias = try (optional alias) <|> pure Nothing
  where
    alias = label "alias" $ assignedFieldName <* ignoredTokens

parseType :: Parser TypeRef
parseType = parseTypeW <$> parseWrappedType <*> parseNonNull

parseTypeW :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> TypeRef
parseTypeW (wrappers, typeConName) nonNull =
  TypeRef
    { typeConName,
      typeArgs = Nothing,
      typeWrappers = toHSWrappers (nonNull <> wrappers)
    }

parseWrappedType :: Parser ([DataTypeWrapper], TypeName)
parseWrappedType = (unwrapped <|> wrapped) <* ignoredTokens
  where
    unwrapped :: Parser ([DataTypeWrapper], TypeName)
    unwrapped = ([],) <$> parseTypeName <* ignoredTokens
    ----------------------------------------------
    wrapped :: Parser ([DataTypeWrapper], TypeName)
    wrapped = brackets (wrapAsList <$> (unwrapped <|> wrapped) <*> parseNonNull)

wrapAsList :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> ([DataTypeWrapper], TypeName)
wrapAsList (wrappers, tName) nonNull = (ListType : nonNull <> wrappers, tName)
