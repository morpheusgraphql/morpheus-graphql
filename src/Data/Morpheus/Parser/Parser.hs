{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parser.Parser
  ( parseGQL
  ) where

import qualified Data.List.NonEmpty                      as NonEmpty
import           Data.Map                                (fromList, toList)
import           Data.Maybe                              (maybe)
import           Data.Morpheus.Parser.Fragment           (fragment)
import           Data.Morpheus.Parser.Internal           (Parser)
import           Data.Morpheus.Parser.Operator           (parseAnonymousQuery, parseOperator)
import           Data.Morpheus.Types.Internal.Validation (GQLError (GQLError), GQLErrors, Validation, desc, positions)
import           Data.Morpheus.Types.Internal.Value      (Value (..))
import           Data.Morpheus.Types.IO                  (GQLRequest (..))
import           Data.Morpheus.Types.Types               (GQLQueryRoot (..))
import           Data.Text                               (Text, pack)
import           Data.Void                               (Void)
import           Text.Megaparsec                         (ParseError, ParseErrorBundle (ParseErrorBundle), SourcePos,
                                                          attachSourcePos, bundleErrors, bundlePosState, eof,
                                                          errorOffset, label, manyTill, parseErrorPretty, runParser,
                                                          (<|>))
import           Text.Megaparsec.Char                    (space)

request :: Parser GQLQueryRoot
request =
  label "GQLQueryRoot" $ do
    space
    operator' <- parseAnonymousQuery <|> parseOperator
    fragmentLib <- fromList <$> manyTill fragment eof
    pure GQLQueryRoot {queryBody = operator', fragments = fragmentLib, inputVariables = []}

processErrorBundle :: ParseErrorBundle Text Void -> GQLErrors
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors
  where
    parseErrorToGQLError :: (ParseError Text Void, SourcePos) -> GQLError
    parseErrorToGQLError (err, position) = GQLError {desc = pack (parseErrorPretty err), positions = [position]}
    bundleToErrors :: ParseErrorBundle Text Void -> [(ParseError Text Void, SourcePos)]
    bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
      NonEmpty.toList $ fst $ attachSourcePos errorOffset bundleErrors bundlePosState

getVariables :: GQLRequest -> [(Text, Value)]
getVariables request' = maybe [] toList (variables request')

parseReq :: GQLRequest -> Either (ParseErrorBundle Text Void) GQLQueryRoot
parseReq requestBody = runParser request "<input>" $ query requestBody

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL requestBody =
  case parseReq requestBody of
    Right root      -> Right $ root {inputVariables = getVariables requestBody}
    Left parseError -> Left $ processErrorBundle parseError
