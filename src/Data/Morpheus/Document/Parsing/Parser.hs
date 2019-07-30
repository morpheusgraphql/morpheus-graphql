{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Parsing.Parser
  ( parseDocument
  ) where

import qualified Data.List.NonEmpty                      as NonEmpty
import           Data.Morpheus.Document.Parsing.DataType (parseDataType)
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataTypeLib (..), defineType, initTypeLib)
import           Data.Text                               (Text, pack)
import           Data.Void                               (Void)
import           Text.Megaparsec                         (ParseError, ParseErrorBundle (ParseErrorBundle), SourcePos,
                                                          attachSourcePos, bundleErrors, bundlePosState, eof,
                                                          errorOffset, label, manyTill, parseErrorPretty, runParser)
import           Text.Megaparsec.Char                    (space)

processErrorBundle :: ParseErrorBundle Text Void -> [Text]
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors
  where
    parseErrorToGQLError :: (ParseError Text Void, SourcePos) -> Text
    parseErrorToGQLError (err, _) = pack (parseErrorPretty err)
    bundleToErrors :: ParseErrorBundle Text Void -> [(ParseError Text Void, SourcePos)]
    bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
      NonEmpty.toList $ fst $ attachSourcePos errorOffset bundleErrors bundlePosState

parseDocument :: Text -> Either [Text] DataTypeLib
parseDocument doc =
  case parseDoc of
    Right root      -> Right root
    Left parseError -> Left $ processErrorBundle parseError
  where
    parseDoc = runParser request "<input>" doc
    request =
      label "GQLQueryRoot" $ do
        space
        dataTypes <- manyTill parseDataType eof
        case lookup "Query" dataTypes of
          Just (OutputObject x) -> pure (foldr defineType (initTypeLib ("Query", x)) withoutQuery)
            where withoutQuery = filter ((/= "Query") . fst) dataTypes
          _ -> fail "Query Not Defined"
