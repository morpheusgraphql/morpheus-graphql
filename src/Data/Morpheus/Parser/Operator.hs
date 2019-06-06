{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Operator
  ( parseAnonymousQuery
  , parseOperator
  ) where

import           Data.Functor                              (($>))
import           Data.Morpheus.Parser.Body                 (entries)
import           Data.Morpheus.Parser.Internal             (Parser)
import           Data.Morpheus.Parser.Primitive            (token, variable)
import           Data.Morpheus.Parser.Terms                (nonNull, parseAssignment, parseMaybeTuple)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..), RawOperator, RawOperator',
                                                            Variable (..))
import           Data.Morpheus.Types.Internal.Data         (DataTypeWrapper (..))
import           Data.Text                                 (Text)
import           Text.Megaparsec                           (between, getSourcePos, label, (<?>), (<|>))
import           Text.Megaparsec.Char                      (char, space, space1, string)

wrapMock :: Parser ([DataTypeWrapper], Text)
wrapMock = do
  mock <- token
  space
  return ([], mock)

insideList :: Parser ([DataTypeWrapper], Text)
insideList =
  between
    (char '[' *> space)
    (char ']' *> space)
    (do (list, name) <- wrapMock <|> insideList
        nonNull' <- nonNull
        return ((ListType : nonNull') ++ list, name))

wrappedSignature :: Parser ([DataTypeWrapper], Text)
wrappedSignature = do
  sig <- insideList <|> wrapMock
  space
  return sig

operatorArgument :: Parser (Text, Variable)
operatorArgument = label "operatorArgument" $ do
  ((name', position'), (wrappers', type')) <- parseAssignment variable wrappedSignature
  nonNull' <- nonNull
  pure
    ( name'
    , Variable
        { variableType = type'
        , isVariableRequired = 0 < length nonNull'
        , variableTypeWrappers = nonNull' ++ wrappers'
        , variablePosition = position'
        })

parseOperator :: Parser RawOperator
parseOperator = label "operator" $ do
  pos <- getSourcePos
  kind' <- operatorKind
  operatorName' <- token
  variables <- parseMaybeTuple operatorArgument
  sel <- entries
  pure (kind' $ Operator' operatorName' variables sel pos)

parseAnonymousQuery :: Parser RawOperator
parseAnonymousQuery = label "AnonymousQuery" $ do
  position' <- getSourcePos
  selection' <- entries
  pure (Query $ Operator' "" [] selection' position') <?> "can't parse AnonymousQuery"

operatorKind :: Parser (RawOperator' -> RawOperator)
operatorKind = label "operatorKind" $ do
  kind <- (string "query" $> Query) <|> (string "mutation" $> Mutation) <|> (string "subscription" $> Subscription)
  space1
  return kind
