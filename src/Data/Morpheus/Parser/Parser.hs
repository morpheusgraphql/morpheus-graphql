{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Parser
  ( parseGQL
  , parseGQLSyntax
  ) where

import qualified Data.List.NonEmpty                      as NonEmpty
import           Data.Map                                (toList)
import           Data.Maybe                              (maybe)
import           Data.Morpheus.Parser.Fragment           (fragment)
import           Data.Morpheus.Parser.Internal           (Parser)
import           Data.Morpheus.Parser.Operator           (parseAnonymousQuery, parseOperator)
import           Data.Morpheus.Types.Internal.Validation (GQLError (GQLError), GQLErrors, Validation, desc, positions)
import           Data.Morpheus.Types.IO                  (GQLRequest (..))
import           Data.Morpheus.Types.Types               (GQLQueryRoot (..))
import           Data.Text                               (Text, pack)
import           Data.Void                               (Void)
import           Text.Megaparsec                         (ParseError, ParseErrorBundle (ParseErrorBundle), SourcePos,
                                                          attachSourcePos, bundleErrors, bundlePosState, eof,
                                                          errorOffset, label, manyTill, parseErrorPretty, runParser,
                                                          (<|>))
import           Text.Megaparsec.Char                    (space)

processErrorBundle :: ParseErrorBundle Text Void -> GQLErrors
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors
  where
    parseErrorToGQLError :: (ParseError Text Void, SourcePos) -> GQLError
    parseErrorToGQLError (err, position) = GQLError {desc = pack (parseErrorPretty err), positions = [position]}
    bundleToErrors :: ParseErrorBundle Text Void -> [(ParseError Text Void, SourcePos)]
    bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
      NonEmpty.toList $ fst $ attachSourcePos errorOffset bundleErrors bundlePosState

parseGQLSyntax :: Text -> Either (ParseErrorBundle Text Void) GQLQueryRoot
parseGQLSyntax = runParser request "<input>"
  where
    request :: Parser GQLQueryRoot
    request =
      label "GQLQueryRoot" $ do
        space
        operator <- parseAnonymousQuery <|> parseOperator
        fragments <- manyTill fragment eof
        pure GQLQueryRoot {operator, fragments, inputVariables = []}

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL GQLRequest {query, variables} =
  case parseGQLSyntax query of
    Right root      -> Right $ root {inputVariables = maybe [] toList variables}
    Left parseError -> Left $ processErrorBundle parseError
