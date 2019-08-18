{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser
  , Position
  , processErrorBundle
  ) where

import qualified Data.List.NonEmpty                      as NonEmpty
import           Data.Morpheus.Types.Internal.Validation (GQLError (..), GQLErrors)
import           Data.Text                               (Text, pack)
import           Data.Void                               (Void)
import           Text.Megaparsec                         (ParseError, ParseErrorBundle (ParseErrorBundle), Parsec,
                                                          SourcePos, SourcePos, attachSourcePos, bundleErrors,
                                                          bundlePosState, errorOffset, parseErrorPretty)

type Position = SourcePos

type Parser = Parsec Void Text

processErrorBundle :: ParseErrorBundle Text Void -> GQLErrors
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors
  where
    parseErrorToGQLError :: (ParseError Text Void, SourcePos) -> GQLError
    parseErrorToGQLError (err, position) =
      GQLError {desc = pack (parseErrorPretty err), positions = [position]}
    bundleToErrors ::
         ParseErrorBundle Text Void -> [(ParseError Text Void, SourcePos)]
    bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
      NonEmpty.toList $
      fst $ attachSourcePos errorOffset bundleErrors bundlePosState
