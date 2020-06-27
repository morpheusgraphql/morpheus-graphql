{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( token,
    qualifier,
    variable,
    ignoredTokens,
    spaceAndComments1,
    pipeLiteral,
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
    litEquals,
    litAssignment,
    parseTuple,
    parseAlias,
    sepByAnd,
    parseName,
    parseType,
    keyword,
    operator,
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
  ( KeyOf,
    Listable (..),
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
    skipMany,
    skipManyTill,
    try,
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

-- Name : https://graphql.github.io/graphql-spec/June2018/#sec-Names
--
-- Name :: /[_A-Za-z][_0-9A-Za-z]*/
--

parseNegativeSign :: Parser Bool
parseNegativeSign = (char '-' $> True <* ignoredTokens) <|> pure False

parseName :: Parser FieldName
parseName = FieldName <$> token

parseTypeName :: Parser TypeName
parseTypeName = label "TypeName" $ TypeName <$> token

keyword :: FieldName -> Parser ()
keyword (FieldName word) = string word *> space1 *> ignoredTokens

operator :: Char -> Parser ()
operator x = char x *> ignoredTokens

-- LITERALS
braces :: Parser [a] -> Parser [a]
braces =
  between
    (char '{' *> ignoredTokens)
    (char '}' *> ignoredTokens)

pipeLiteral :: Parser ()
pipeLiteral = char '|' *> ignoredTokens

litEquals :: Parser ()
litEquals = char '=' *> ignoredTokens

litAssignment :: Parser ()
litAssignment = char ':' *> ignoredTokens

-- PRIMITIVE
------------------------------------
token :: Parser Token
token = label "token" $ do
  firstChar <- letterChar <|> char '_'
  restToken <- many $ letterChar <|> char '_' <|> digitChar
  ignoredTokens
  return $ pack $ firstChar : restToken

qualifier :: Parser (FieldName, Position)
qualifier = label "qualifier" $ do
  position <- getLocation
  value <- parseName
  return (value, position)

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

spaceAndComments1 :: Parser ()
spaceAndComments1 = space1 *> ignoredTokens

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
      *> skipMany inlineComment
      *> space
  where
    inlineComment =
      label "Comment" $
        char '#' *> skipManyTill printChar newline *> space

------------------------------------------------------------------------

-- COMPLEX
sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional (char '&') *> ignoredTokens)

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` many (char ',' *> ignoredTokens))

setOf :: (Listable a coll, KeyOf a) => Parser a -> Parser coll
setOf = collection >=> fromElems

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  ignoredTokens
  return wrapper

optionalList :: Parser [a] -> Parser [a]
optionalList x = x <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
    between
      (char '(' *> ignoredTokens)
      (char ')' *> ignoredTokens)
      ( parser `sepBy` (many (char ',') *> ignoredTokens) <?> "empty Tuple value!"
      )

uniqTuple :: (Listable a coll, KeyOf a) => Parser a -> Parser coll
uniqTuple = parseTuple >=> fromElems

uniqTupleOpt :: (Listable a coll, KeyOf a) => Parser a -> Parser coll
uniqTupleOpt = optionalList . parseTuple >=> fromElems

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
  _ <- string "on"
  space1
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
