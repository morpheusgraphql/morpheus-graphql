{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
    processParser,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Types.Internal.AST
  ( GQLError (..),
    GQLErrors,
    Position (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
    failure,
  )
import Data.Text
  ( Text,
  )
import Data.Void (Void)
import Text.Megaparsec
  ( ParseError,
    ParseErrorBundle
      ( ParseErrorBundle
      ),
    ParsecT,
    SourcePos,
    SourcePos (..),
    attachSourcePos,
    bundleErrors,
    bundlePosState,
    errorOffset,
    getSourcePos,
    parseErrorPretty,
    runParserT,
    unPos,
  )

getLocation :: Parser Position
getLocation = fmap toLocation getSourcePos

toLocation :: SourcePos -> Position
toLocation SourcePos {sourceLine, sourceColumn} =
  Position {line = unPos sourceLine, column = unPos sourceColumn}

type MyError = Void

type Parser = ParsecT MyError Text Eventless

type ErrorBundle = ParseErrorBundle Text MyError

processParser :: Parser a -> Text -> Eventless a
processParser parser txt = case runParserT parser [] txt of
  Success {result} -> case result of
    Right root -> pure root
    Left parseError -> failure (processErrorBundle parseError)
  Failure {errors} -> failure errors

processErrorBundle :: ErrorBundle -> GQLErrors
processErrorBundle = map parseErrorToGQLError . bundleToErrors
  where
    parseErrorToGQLError :: (ParseError Text MyError, SourcePos) -> GQLError
    parseErrorToGQLError (err, position) =
      GQLError
        { message = msg (parseErrorPretty err),
          locations = [toLocation position]
        }
    bundleToErrors ::
      ErrorBundle -> [(ParseError Text MyError, SourcePos)]
    bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
      NonEmpty.toList $ fst $
        attachSourcePos
          errorOffset
          bundleErrors
          bundlePosState
