{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( name,
    variable,
    ignoredTokens,
    -------------
    collection,
    setOf,
    uniqTuple,
    uniqTupleOpt,
    parseTypeCondition,
    spreadLiteral,
    parseNonNull,
    parseAssignment,
    parseWrappedType,
    litAssignment,
    parseAlias,
    sepByAnd,
    parseName,
    parseType,
    keyword,
    symbol,
    optDescription,
    optionalList,
    parseNegativeSign,
    parseTypeName,
  )
where

import Control.Monad ((>=>))
import Data.Functor (($>))
-- MORPHEUS

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
  ( pack,
    strip,
  )
import Text.Megaparsec
  ( (<?>),
    (<|>),
    between,
    label,
    many,
    manyTill,
    optional,
    sepBy,
    sepEndBy,
    skipManyTill,
    try,
  )
import Text.Megaparsec.Char
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
parseNegativeSign = (char '-' $> True <* ignoredTokens) <|> pure False

parseName :: Parser FieldName
parseName = FieldName <$> name

parseTypeName :: Parser TypeName
parseTypeName = label "TypeName" $ TypeName <$> name

keyword :: FieldName -> Parser ()
keyword (FieldName word) = string word *> space1 *> ignoredTokens

symbol :: Char -> Parser ()
symbol x = char x *> ignoredTokens

-- LITERALS
braces :: Parser [a] -> Parser [a]
braces =
  between
    (char '{' *> ignoredTokens)
    (char '}' *> ignoredTokens)

litAssignment :: Parser ()
litAssignment = char ':' *> ignoredTokens

-- PRIMITIVE
------------------------------------

-- 2.1.9 Names
-- https://spec.graphql.org/draft/#Name
-- Name ::
--  NameStart NameContinue[list,opt]
--
name :: Parser Token
name = label "token" $ do
  start <- nameStart
  continue <- nameContinue
  ignoredTokens
  pure $ pack (start : continue)

-- NameStart::
--   Letter
--   _
nameStart :: Parser Char
nameStart = letterChar <|> char '_'

--  NameContinue::
--   Letter
--   Digit
nameContinue :: Parser String
nameContinue = many (letterChar <|> char '_' <|> digitChar)

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Parser Ref
variable = label "variable" $ do
  refPosition <- getLocation
  _ <- char '$'
  refName <- parseName
  ignoredTokens
  pure $ Ref {refName, refPosition}

-- Descriptions: https://graphql.github.io/graphql-spec/June2018/#Description
--
-- Description:
--   StringValue
-- TODO: should support """ and "
--
optDescription :: Parser (Maybe Description)
optDescription = optional parseDescription

parseDescription :: Parser Description
parseDescription =
  strip . pack <$> (blockDescription <|> singleLine) <* ignoredTokens
  where
    blockDescription =
      blockQuotes
        *> manyTill (printChar <|> newline) blockQuotes
        <* ignoredTokens
      where
        blockQuotes = string "\"\"\""
    ----------------------------
    singleLine =
      stringQuote *> manyTill printChar stringQuote <* ignoredTokens
      where
        stringQuote = char '"'

-- Ignored Tokens : https://graphql.github.io/graphql-spec/June2018/#sec-Source-Text.Ignored-Tokens
--  Ignored:
--    UnicodeBOM
--    WhiteSpace
--    LineTerminator
--    Comment
--    Comma
-- TODO: implement as in specification
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
    char '#' *> skipManyTill printChar newline *> space

comma :: Parser ()
comma = label "Comma" $ char ',' *> space

------------------------------------------------------------------------
-- COMPLEX
sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional (char '&') *> ignoredTokens)

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)

setOf :: (Listable a coll, KeyOf a) => Parser a -> Parser coll
setOf = collection >=> fromElems

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  ignoredTokens
  return wrapper

optionalList :: Parser [a] -> Parser [a]
optionalList x = x <|> pure []

uniqTuple :: (Listable a coll, KeyOf a) => Parser a -> Parser coll
uniqTuple parser =
  label "Tuple" $
    between
      (char '(' *> ignoredTokens)
      (char ')' *> ignoredTokens)
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= fromElems

uniqTupleOpt :: (Listable a coll, Collection a coll, KeyOf a) => Parser a -> Parser coll
uniqTupleOpt x = uniqTuple x <|> pure empty

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser valueParser = label "assignment" $ do
  name' <- nameParser
  litAssignment
  value' <- valueParser
  pure (name', value')

-- Type Conditions: https://graphql.github.io/graphql-spec/June2018/#sec-Type-Conditions
--
--  TypeCondition:
--    on NamedType
--
parseTypeCondition :: Parser TypeName
parseTypeCondition = do
  keyword "on"
  parseTypeName

spreadLiteral :: Parser Position
spreadLiteral = do
  index <- getLocation
  _ <- string "..."
  space
  return index

parseWrappedType :: Parser ([DataTypeWrapper], TypeName)
parseWrappedType = (unwrapped <|> wrapped) <* ignoredTokens
  where
    unwrapped :: Parser ([DataTypeWrapper], TypeName)
    unwrapped = ([],) <$> parseTypeName <* ignoredTokens
    ----------------------------------------------
    wrapped :: Parser ([DataTypeWrapper], TypeName)
    wrapped =
      between
        (char '[' *> ignoredTokens)
        (char ']' *> ignoredTokens)
        ( do
            (wrappers, name) <- unwrapped <|> wrapped
            nonNull' <- parseNonNull
            return ((ListType : nonNull') ++ wrappers, name)
        )

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: Parser (Maybe FieldName)
parseAlias = try (optional alias) <|> pure Nothing
  where
    alias = label "alias" $ parseName <* char ':' <* ignoredTokens

parseType :: Parser TypeRef
parseType = do
  (wrappers, typeConName) <- parseWrappedType
  nonNull <- parseNonNull
  pure
    TypeRef
      { typeConName,
        typeArgs = Nothing,
        typeWrappers = toHSWrappers $ nonNull ++ wrappers
      }
