{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Request.Parser
  ( parseGQL
  ) where

import qualified Data.Aeson                              as Aeson (Value (..))
import           Data.HashMap.Lazy                       (toList)
import           Data.Text                               (Text)
import           Data.Void                               (Void)
import           Text.Megaparsec                         (ParseErrorBundle, eof, label, manyTill, runParser, (<|>))
import           Text.Megaparsec.Char                    (space)

--
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal (Parser, processErrorBundle)
import           Data.Morpheus.Parsing.Request.Fragment  (fragment)
import           Data.Morpheus.Parsing.Request.Operation (parseAnonymousQuery, parseOperation)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Internal.Value      (Value (..), replaceValue)
import           Data.Morpheus.Types.IO                  (GQLRequest (..))
import           Data.Morpheus.Types.Types               (GQLQueryRoot (..))

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
