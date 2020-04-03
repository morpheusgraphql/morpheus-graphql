{-# LANGUAGE NamedFieldPuns             #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser
  , Position
  , getLocation
  , processParser
  )
where

import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Morpheus.Error.Utils      ( toLocation )
import           Data.Morpheus.Types.Internal.AST
                                                ( Position 
                                                , GQLError(..)
                                                , GQLErrors
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Stateless
                                                , failure
                                                , Result(..)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Text.Megaparsec                ( ParseError
                                                , ParseErrorBundle
                                                  ( ParseErrorBundle
                                                  )
                                                , ParsecT
                                                , SourcePos
                                                , attachSourcePos
                                                , bundleErrors
                                                , bundlePosState
                                                , errorOffset
                                                , getSourcePos
                                                , parseErrorPretty
                                                , runParserT
                                                )
import           Data.Void                      (Void)

getLocation :: Parser Position
getLocation = fmap toLocation getSourcePos

type MyError = Void
type Parser = ParsecT MyError Text Stateless
type ErrorBundle = ParseErrorBundle Text MyError

processParser :: Parser a -> Text -> Stateless a
processParser parser txt = case runParserT parser [] txt of
  Success { result } -> case result of
    Right root       -> pure root
    Left  parseError -> failure (processErrorBundle parseError) 
  Failure { errors } -> failure errors

processErrorBundle :: ErrorBundle -> GQLErrors
processErrorBundle = map parseErrorToGQLError . bundleToErrors
 where
  parseErrorToGQLError :: (ParseError Text MyError, SourcePos) -> GQLError
  parseErrorToGQLError (err, position) = GQLError
    { message   = pack (parseErrorPretty err)
      , locations = [toLocation position]
    }
  bundleToErrors
    :: ErrorBundle -> [(ParseError Text MyError, SourcePos)]
  bundleToErrors ParseErrorBundle { bundleErrors, bundlePosState } =
    NonEmpty.toList $ fst $ attachSourcePos errorOffset
                                            bundleErrors
                                            bundlePosState