{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( token
  , qualifier
  , variable
  , spaceAndComments
  , spaceAndComments1
  , pipeLiteral
  -------------
  , setOf
  , parseTypeCondition
  , spreadLiteral
  , parseNonNull
  , parseMaybeTuple
  , parseAssignment
  , parseWrappedType
  , litEquals
  , litAssignment
  , parseTuple
  , parseAlias
  , sepByAnd
  , parseName
  , parseType
  , keyword
  , operator
  , optDescription
  , parseNegativeSign
  )
where

import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text
                                                , pack
                                                , strip
                                                )
import           Text.Megaparsec                ( between
                                                , label
                                                , try
                                                , many
                                                , manyTill
                                                , optional
                                                , sepBy
                                                , sepEndBy
                                                , skipMany
                                                , skipManyTill
                                                , try
                                                , (<?>)
                                                , (<|>)
                                                )
import           Text.Megaparsec.Char           ( char
                                                , digitChar
                                                , letterChar
                                                , newline
                                                , printChar
                                                , space
                                                , space1
                                                , string
                                                )

-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( Parser
                                                , Position
                                                , getLocation
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataTypeWrapper(..)
                                                , Key
                                                , Description
                                                , Name
                                                , toHSWrappers
                                                , convertToHaskellName
                                                , Ref(..)
                                                , TypeRef(..)
                                                )


-- Name : https://graphql.github.io/graphql-spec/June2018/#sec-Names
--
-- Name :: /[_A-Za-z][_0-9A-Za-z]*/
--

parseNegativeSign :: Parser Bool
parseNegativeSign = (char '-' $> True <* spaceAndComments) <|> pure False

parseName :: Parser Name
parseName = token

keyword :: Key -> Parser ()
keyword word = string word *> space1 *> spaceAndComments

operator :: Char -> Parser ()
operator x = char x *> spaceAndComments

-- LITERALS
setLiteral :: Parser [a] -> Parser [a]
setLiteral =
  between (char '{' *> spaceAndComments) (char '}' *> spaceAndComments)

pipeLiteral :: Parser ()
pipeLiteral = char '|' *> spaceAndComments

litEquals :: Parser ()
litEquals = char '=' *> spaceAndComments

litAssignment :: Parser ()
litAssignment = char ':' *> spaceAndComments

-- PRIMITIVE
------------------------------------
token :: Parser Text
token = label "token" $ do
  firstChar <- letterChar <|> char '_'
  restToken <- many $ letterChar <|> char '_' <|> digitChar
  spaceAndComments
  return $ convertToHaskellName $ pack $ firstChar : restToken

qualifier :: Parser (Text, Position)
qualifier = label "qualifier" $ do
  position <- getLocation
  value    <- token
  return (value, position)


-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Parser Ref
variable = label "variable" $ do
  refPosition <- getLocation
  _           <- char '$'
  refName     <- token
  spaceAndComments
  pure $ Ref { refName, refPosition }

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

parseDescription :: Parser Text
parseDescription =
  strip . pack <$> (blockDescription <|> singleLine) <* spaceAndComments
 where
  blockDescription =
    blockQuotes
      *> manyTill (printChar <|> newline) blockQuotes
      <* spaceAndComments
    where blockQuotes = string "\"\"\""
  ----------------------------
  singleLine =
    stringQuote *> manyTill printChar stringQuote <* spaceAndComments
    where stringQuote = char '"'

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
  where inlineComment = char '#' *> skipManyTill printChar newline *> space
    ------------------------------------------------------------------------

-- COMPLEX
sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (char '&' *> spaceAndComments)

-----------------------------
setOf :: Parser a -> Parser [a]
setOf entry = setLiteral (entry `sepEndBy` many (char ',' *> spaceAndComments))

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  spaceAndComments
  return wrapper

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser = label "Tuple" $ between
  (char '(' *> spaceAndComments)
  (char ')' *> spaceAndComments)
  (parser `sepBy` (many (char ',') *> spaceAndComments) <?> "empty Tuple value!"
  )

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
parseTypeCondition :: Parser Text
parseTypeCondition = do
  _ <- string "on"
  space1
  token

spreadLiteral :: Parser Position
spreadLiteral = do
  index <- getLocation
  _     <- string "..."
  space
  return index

parseWrappedType :: Parser ([DataTypeWrapper], Text)
parseWrappedType = (unwrapped <|> wrapped) <* spaceAndComments
 where
  unwrapped :: Parser ([DataTypeWrapper], Text)
  unwrapped = ([], ) <$> token <* spaceAndComments
  ----------------------------------------------
  wrapped :: Parser ([DataTypeWrapper], Text)
  wrapped = between
    (char '[' *> spaceAndComments)
    (char ']' *> spaceAndComments)
    (do
      (wrappers, name) <- unwrapped <|> wrapped
      nonNull'         <- parseNonNull
      return ((ListType : nonNull') ++ wrappers, name)
    )

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: Parser (Maybe Key)
parseAlias = try (optional alias) <|> pure Nothing
  where alias = label "alias" $ token <* char ':' <* spaceAndComments


parseType :: Parser TypeRef
parseType = do
  (wrappers, typeConName) <- parseWrappedType
  nonNull                 <- parseNonNull
  pure TypeRef { typeConName
               , typeArgs     = Nothing
               , typeWrappers = toHSWrappers $ nonNull ++ wrappers
               }
