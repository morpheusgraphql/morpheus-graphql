{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( token,
    qualifier,
    variable,
    spaceAndComments,
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
  )
where

import Control.Monad ((>=>))
import Data.Functor (($>))
-- MORPHEUS

import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataTypeWrapper (..),
    Description,
    FieldName,
    Name (..),
    Ref (..),
    Token,
    TypeName,
    TypeRef (..),
    convertToHaskellName,
    toHSWrappers,
  )
import Data.Morpheus.Types.Internal.Operation
  ( KeyOf,
    Listable (..),
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
parseNegativeSign = (char '-' $> True <* spaceAndComments) <|> pure False

parseName :: Parser Name
parseName = token

keyword :: Name -> Parser ()
keyword (Name word) = string word *> space1 *> spaceAndComments

operator :: Char -> Parser ()
operator x = char x *> spaceAndComments

-- LITERALS
braces :: Parser [a] -> Parser [a]
braces =
  between
    (char '{' *> spaceAndComments)
    (char '}' *> spaceAndComments)

pipeLiteral :: Parser ()
pipeLiteral = char '|' *> spaceAndComments

litEquals :: Parser ()
litEquals = char '=' *> spaceAndComments

litAssignment :: Parser ()
litAssignment = char ':' *> spaceAndComments

-- PRIMITIVE
------------------------------------
token :: Parser Name
token = label "token" $ do
  firstChar <- letterChar <|> char '_'
  restToken <- many $ letterChar <|> char '_' <|> digitChar
  spaceAndComments
  return $ convertToHaskellName $ Name $ pack $ firstChar : restToken

qualifier :: Parser (Name, Position)
qualifier = label "qualifier" $ do
  position <- getLocation
  value <- token
  return (value, position)

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Parser Ref
variable = label "variable" $ do
  refPosition <- getLocation
  _ <- char '$'
  refName <- token
  spaceAndComments
  pure $ Ref {refName, refPosition}

spaceAndComments1 :: Parser ()
spaceAndComments1 = space1 *> spaceAndComments

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
  strip . pack <$> (blockDescription <|> singleLine) <* spaceAndComments
  where
    blockDescription =
      blockQuotes
        *> manyTill (printChar <|> newline) blockQuotes
        <* spaceAndComments
      where
        blockQuotes = string "\"\"\""
    ----------------------------
    singleLine =
      stringQuote *> manyTill printChar stringQuote <* spaceAndComments
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
spaceAndComments :: Parser ()
spaceAndComments = ignoredTokens

ignoredTokens :: Parser ()
ignoredTokens =
  label "IgnoredTokens" $ space *> skipMany inlineComment *> space
  where
    inlineComment = char '#' *> skipManyTill printChar newline *> space

------------------------------------------------------------------------

-- COMPLEX
sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional (char '&') *> spaceAndComments)

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` many (char ',' *> spaceAndComments))

setOf :: (Listable c a, KeyOf a) => Parser a -> Parser c
setOf = collection >=> fromList

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  spaceAndComments
  return wrapper

optionalList :: Parser [a] -> Parser [a]
optionalList x = x <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
    between
      (char '(' *> spaceAndComments)
      (char ')' *> spaceAndComments)
      ( parser `sepBy` (many (char ',') *> spaceAndComments) <?> "empty Tuple value!"
      )

uniqTuple :: (Listable c a, KeyOf a) => Parser a -> Parser c
uniqTuple = parseTuple >=> fromList

uniqTupleOpt :: (Listable c a, KeyOf a) => Parser a -> Parser c
uniqTupleOpt = optionalList . parseTuple >=> fromList

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
  token

spreadLiteral :: Parser Position
spreadLiteral = do
  index <- getLocation
  _ <- string "..."
  space
  return index

parseWrappedType :: Parser ([DataTypeWrapper], TypeName)
parseWrappedType = (unwrapped <|> wrapped) <* spaceAndComments
  where
    unwrapped :: Parser ([DataTypeWrapper], TypeName)
    unwrapped = ([],) <$> token <* spaceAndComments
    ----------------------------------------------
    wrapped :: Parser ([DataTypeWrapper], TypeName)
    wrapped =
      between
        (char '[' *> spaceAndComments)
        (char ']' *> spaceAndComments)
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
    alias = label "alias" $ token <* char ':' <* spaceAndComments

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
