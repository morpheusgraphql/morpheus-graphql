{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Morpheus.Document.Parsing.Terms
  ( nonNull
  , parseMaybeTuple
  , parseAssignment
  , token
  , qualifier
  , pipe
  , wrappedType
  , setOf
  , spaceAndComments
  , Parser
  , Position
  ) where

import           Data.Functor                       (($>))
import           Data.Morpheus.Types.Internal.Data  (DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Value (convertToHaskellName)
import           Data.Text                          (Text)
import qualified Data.Text                          as T (pack)
import           Data.Void                          (Void)
import           Text.Megaparsec                    (Parsec, SourcePos, between, getSourcePos, label, many, sepBy,
                                                     sepEndBy, skipMany, skipManyTill, (<?>), (<|>))
import           Text.Megaparsec.Char               (char, digitChar, letterChar, newline, printChar, space)

type Position = SourcePos

type Parser = Parsec Void Text

spaceAndComments :: Parser ()
spaceAndComments = space *> skipMany inlineComment *> space
  where
    inlineComment = char '#' *> skipManyTill printChar newline *> space

setOf :: Parser a -> Parser [a]
setOf entry = setLiteral (entry `sepEndBy` many (char ',' *> spaceAndComments))

setLiteral :: Parser [a] -> Parser [a]
setLiteral = between (char '{' *> spaceAndComments) (char '}' *> spaceAndComments)

pipe :: Parser ()
pipe = char '|' *> spaceAndComments

nonNull :: Parser [DataTypeWrapper]
nonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  spaceAndComments
  return wrapper

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
  between
    (char '(' *> spaceAndComments)
    (char ')' *> spaceAndComments)
    (parser `sepBy` (many (char ',') *> spaceAndComments) <?> "empty Tuple value!")

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' =
  label "assignment" $ do
    name' <- nameParser'
    char ':' *> spaceAndComments
    value' <- valueParser'
    pure (name', value')

token :: Parser Text
token =
  label "token" $ do
    firstChar <- letterChar <|> char '_'
    restToken <- many $ letterChar <|> char '_' <|> digitChar
    spaceAndComments
    return $ convertToHaskellName $ T.pack $ firstChar : restToken

qualifier :: Parser (Text, Position)
qualifier =
  label "qualifier" $ do
    position' <- getSourcePos
    value <- token
    return (value, position')

wrappedType :: Parser ([DataTypeWrapper], Text)
wrappedType = (unwrapped <|> wrapped) <* spaceAndComments
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
            nonNull' <- nonNull
            return ((ListType : nonNull') ++ wrappers, name))
