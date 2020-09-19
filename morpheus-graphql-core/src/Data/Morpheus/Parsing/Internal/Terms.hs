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
  )
where

import Control.Monad ((>=>))
-- MORPHEUS
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
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
    toHSWrappers,
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
parseNegativeSign = (char "-" $> True <* ignoredTokens) <|> pure False

parseName :: Parser FieldName
parseName = FieldName <$> name

parseTypeName :: Parser TypeName
parseTypeName = label "TypeName" $ TypeName <$> name

keyword :: FieldName -> Parser ()
keyword (FieldName word) = string word *> space1 *> ignoredTokens

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens

-- LITERALS
braces :: Parser [a] -> Parser [a]
braces =
  between
    (char "{" *> ignoredTokens)
    (char "}" *> ignoredTokens)

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
    pack
      <$> ((:) <$> nameStart <*> nameContinue)
      <* ignoredTokens

_' :: Parser Word8
_' = char "_"

-- NameStart::
--   Letter
--   _
nameStart :: Parser Word8
nameStart = letterChar <|> _'

--  NameContinue::
--   Letter
--   Digit
nameContinue :: Parser [Word8]
nameContinue = many (letterChar <|> _' <|> digitChar)

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Parser Ref
variable =
  label "variable" $
    (flip Ref <$> getLocation <*> (char "$" *> parseName))
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
singleLineString = stringWith (char "\"") escapedChar

stringWith :: Parser quote -> Parser Char -> Parser Token
stringWith quote parser =
  pack
    <$> ( quote
            *> manyTill parser quote
            <* ignoredTokens
        )

escapedChar :: Parser Char
escapedChar = label "EscapedChar" $ printChar >>= handleEscape

handleEscape :: Char -> Parser Char
handleEscape '\\' = choice escape
handleEscape x = pure x

escape :: [Parser Char]
escape = map escapeCh escapeOptions
  where
    escapeCh :: (Char, Char) -> Parser Char
    escapeCh (code, replacement) = char code $> replacement

escapeOptions :: [(Char, Char)]
escapeOptions =
  [ ('b', '\b'),
    ('n', '\n'),
    ('f', '\f'),
    ('r', '\r'),
    ('t', '\t'),
    ('\\', '\\'),
    ('\"', '\"'),
    ('/', '/')
  ]

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
comment =
  label "Comment" $
    char "#" *> skipManyTill printChar newline *> space

comma :: Parser ()
comma = label "Comma" $ char "," *> space

------------------------------------------------------------------------

sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional (symbol "&") *> ignoredTokens)

pipe :: Parser a -> Parser [a]
pipe x = optional (symbol "|") *> (x `sepBy1` symbol "|")

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)

setOf :: (Listable a coll, KeyOf k a) => Parser a -> Parser coll
setOf = collection >=> lift . fromElems

optionalCollection :: Collection a c => Parser c -> Parser c
optionalCollection x = x <|> pure empty

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull =
  ((char "!" $> [NonNullType]) <|> pure [])
    <* ignoredTokens

uniqTuple :: (Listable a coll, KeyOf k a) => Parser a -> Parser coll
uniqTuple parser =
  label "Tuple" $
    between
      (char "(" *> ignoredTokens)
      (char ")" *> ignoredTokens)
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= lift . fromElems

uniqTupleOpt :: (Listable a coll, Collection a coll, KeyOf k a) => Parser a -> Parser coll
uniqTupleOpt x = uniqTuple x <|> pure empty

assignedFieldName :: Parser FieldName
assignedFieldName = parseName <* symbol ":"

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
    wrapped =
      between
        (char "[" *> ignoredTokens)
        (char "]" *> ignoredTokens)
        (wrapAsList <$> (unwrapped <|> wrapped) <*> parseNonNull)

wrapAsList :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> ([DataTypeWrapper], TypeName)
wrapAsList (wrappers, tName) nonNull = (ListType : nonNull <> wrappers, tName)
