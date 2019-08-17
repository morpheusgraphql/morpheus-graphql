{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Parser
  ( parseGQL
  ) where

import qualified Data.Aeson                              as Aeson (Value (..))
import           Data.HashMap.Lazy                       (toList)
import qualified Data.List.NonEmpty                      as NonEmpty
import           Data.Morpheus.Parser.Fragment           (fragment)
import           Data.Morpheus.Parser.Internal           (Parser)
import           Data.Morpheus.Parser.Operation          (parseAnonymousQuery, parseOperation)
import           Data.Morpheus.Types.Internal.Validation (GQLError (GQLError), GQLErrors, Validation, desc, positions)
import           Data.Morpheus.Types.Internal.Value      (Value (..), replaceValue)
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
        operation <- parseAnonymousQuery <|> parseOperation
        fragments <- manyTill fragment eof
        pure GQLQueryRoot {operation, fragments, inputVariables = []}

toVariableMap :: Maybe Aeson.Value -> [(Text, Value)]
toVariableMap (Just (Aeson.Object x)) = map toMorpheusValue (toList x)
  where
    toMorpheusValue (key, value) = (key, replaceValue value)
toVariableMap _ = []

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL GQLRequest {query, variables} =
  case parseGQLSyntax query of
    Right root      -> Right $ root {inputVariables = toVariableMap variables}
    Left parseError -> Left $ processErrorBundle parseError
