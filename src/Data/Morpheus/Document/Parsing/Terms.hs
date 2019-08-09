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
  , ignoreComments
  , Parser
  , Position
  ) where

import           Data.Functor                       (($>))
import           Data.Morpheus.Types.Internal.Data  (DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Value (convertToHaskellName)
import           Data.Text                          (Text)
import qualified Data.Text                          as T (pack)
import           Data.Void                          (Void)
import           Text.Megaparsec                    (Parsec, SourcePos, between, getSourcePos, label, many, option,
                                                     sepBy, sepEndBy, skipMany, skipManyTill, (<?>), (<|>))
import           Text.Megaparsec.Char               (char, digitChar, letterChar, newline, printChar, space)

type Position = SourcePos

type Parser = Parsec Void Text

ignoreComments :: Parser ()
ignoreComments = space *> skipMany inlineComment *> space
  where
    inlineComment = char '#' *> skipManyTill printChar newline *> space

setOf :: Parser a -> Parser [a]
setOf entry = setLiteral (entry `sepEndBy` many (char ',' *> ignoreComments))

setLiteral :: Parser [a] -> Parser [a]
setLiteral = between (char '{' *> ignoreComments) (char '}' *> ignoreComments)

pipe :: Parser ()
pipe = char '|' *> ignoreComments

nonNull :: Parser [DataTypeWrapper]
nonNull = do
  wrapper <- (char '!' $> [NonNullType]) <|> pure []
  ignoreComments
  return wrapper

parseMaybeTuple :: Parser a -> Parser [a]
parseMaybeTuple parser = parseTuple parser <|> pure []

parseTuple :: Parser a -> Parser [a]
parseTuple parser =
  label "Tuple" $
  between
    (char '(' *> ignoreComments)
    (char ')' *> ignoreComments)
    (parser `sepBy` (many (char ',') *> ignoreComments) <?> "empty Tuple value!")

parseAssignment :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, b)
parseAssignment nameParser' valueParser' =
  label "assignment" $ do
    name' <- nameParser'
    char ':' *> ignoreComments
    value' <- valueParser'
    pure (name', value')

token :: Parser Text
token =
  label "token" $ do
    firstChar <- letterChar <|> char '_'
    restToken <- many $ letterChar <|> char '_' <|> digitChar
    ignoreComments
    return $ convertToHaskellName $ T.pack $ firstChar : restToken

qualifier :: Parser (Text, Position)
qualifier =
  label "qualifier" $ do
    position' <- getSourcePos
    value <- token
    return (value, position')

wrappedType :: Parser ([DataTypeWrapper], Text)
wrappedType = (unwrapped <|> wrapped) <* ignoreComments
  where
    unwrapped :: Parser ([DataTypeWrapper], Text)
    unwrapped = ([], ) <$> token <* ignoreComments
    ----------------------------------------------
    wrapped :: Parser ([DataTypeWrapper], Text)
    wrapped =
      between
        (char '[' *> ignoreComments)
        (char ']' *> ignoreComments)
        (do (wrappers, name) <- unwrapped <|> wrapped
            nonNull' <- nonNull
            return ((ListType : nonNull') ++ wrappers, name))
