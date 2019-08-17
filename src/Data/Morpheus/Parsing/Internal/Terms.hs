{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( token
  , qualifier
  , variable
  , spaceAndComments
  -------------
  , onType
  , spreadLiteral
  , parseNonNull
  , parseMaybeTuple
  , parseAssignment
  , parseWrappedType
  ) where

import           Data.Functor                            (($>))
import           Data.Morpheus.Parsing.Internal.Internal (Parser, Position)
import           Data.Morpheus.Types.Internal.Data       (DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Value      (convertToHaskellName)
import           Data.Text                               (Text, pack)
import           Text.Megaparsec                         (between, getSourcePos, label, many, sepBy, skipMany,
                                                          skipManyTill, (<?>), (<|>))
import           Text.Megaparsec.Char                    (char, digitChar, letterChar, newline, printChar, space,
                                                          space1, string)

-- PRIMITIVE
------------------------------------
token :: Parser Text
token =
  label "token" $ do
    firstChar <- letterChar <|> char '_'
    restToken <- many $ letterChar <|> char '_' <|> digitChar
    spaceAndComments
    return $ convertToHaskellName $ pack $ firstChar : restToken

qualifier :: Parser (Text, Position)
qualifier =
  label "qualifier" $ do
    position <- getSourcePos
    value <- token
    return (value, position)

variable :: Parser (Text, Position)
variable =
  label "variable" $ do
    position' <- getSourcePos
    _ <- char '$'
    varName' <- token
    return (varName', position')

spaceAndComments :: Parser ()
spaceAndComments = space *> skipMany inlineComment *> space
  where
    inlineComment = char '#' *> skipManyTill printChar newline *> space

-- COMPLEX
-----------------------------
parseNonNull :: Parser [DataTypeWrapper]
parseNonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  spaceAndComments
  return wrapper

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
  between (char '(' *> space) (char ')' *> space) (parser `sepBy` (char ',' *> space) <?> "empty Tuple value!")

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' =
  label "assignment" $ do
    name' <- nameParser'
    char ':' *> space
    value' <- valueParser'
    pure (name', value')

onType :: Parser Text
onType = do
  _ <- string "on"
  space1
  token

spreadLiteral :: Parser Position
spreadLiteral = do
  index <- getSourcePos
  _ <- string "..."
  space
  return index

parseWrappedType :: Parser ([DataTypeWrapper], Text)
parseWrappedType = (unwrapped <|> wrapped) <* spaceAndComments
  where
    unwrapped :: Parser ([DataTypeWrapper], Text)
    unwrapped = ([], ) <$> token <* spaceAndComments
    ----------------------------------------------
    wrapped :: Parser ([DataTypeWrapper], Text)
    wrapped =
      between
        (char '[' *> spaceAndComments)
        (char ']' *> spaceAndComments)
        (do (wrappers, name) <- unwrapped <|> wrapped
            nonNull' <- parseNonNull
            return ((ListType : nonNull') ++ wrappers, name))
