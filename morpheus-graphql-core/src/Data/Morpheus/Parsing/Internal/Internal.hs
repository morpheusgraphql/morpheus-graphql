{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
    processParser,
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Ext.Result
  ( Eventless,
    Result (..),
    failure,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError (..),
    GQLErrors,
    Position (..),
    msg,
  )
import Relude
  ( ($),
    (.),
    Applicative (..),
    Either (..),
    Functor (..),
    Void,
    fst,
  )
import Text.Megaparsec
  ( ParseError,
    ParseErrorBundle
      ( ParseErrorBundle
      ),
    ParsecT,
    SourcePos,
    SourcePos (..),
    Stream,
    attachSourcePos,
    bundleErrors,
    bundlePosState,
    errorOffset,
    getSourcePos,
    parseErrorPretty,
    runParserT,
    unPos,
  )

getLocation :: Stream s => Parser s Position
getLocation = fmap toLocation getSourcePos
{-# INLINEABLE getLocation #-}

toLocation :: SourcePos -> Position
toLocation SourcePos {sourceLine, sourceColumn} =
  Position {line = unPos sourceLine, column = unPos sourceColumn}
{-# INLINEABLE toLocation #-}

type MyError = Void

type Parser s = ParsecT MyError s Eventless

type ErrorBundle s = ParseErrorBundle s MyError

processParser :: Stream s => Parser s a -> s -> Eventless a
processParser parser txt = case runParserT parser [] txt of
  Success {result} -> case result of
    Right root -> pure root
    Left parseError -> failure (processErrorBundle parseError)
  Failure {errors} -> failure errors

processErrorBundle :: Stream s => ErrorBundle s -> GQLErrors
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors

parseErrorToGQLError :: Stream s => (ParseError s MyError, SourcePos) -> GQLError
parseErrorToGQLError (err, position) =
  GQLError
    { message = msg (parseErrorPretty err),
      locations = [toLocation position]
    }

bundleToErrors ::
  Stream s => ErrorBundle s -> [(ParseError s MyError, SourcePos)]
bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
  NonEmpty.toList $ fst $
    attachSourcePos
      errorOffset
      bundleErrors
      bundlePosState
