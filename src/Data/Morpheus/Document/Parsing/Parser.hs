{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Parsing.Parser
  ( parseDocument
  ) where

import qualified Data.List.NonEmpty                      as NonEmpty
import           Data.Morpheus.Document.Parsing.DataType (parseDataType)
import           Data.Morpheus.Document.Parsing.Terms    (ignoreComments)
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataTypeLib (..), defineType, initTypeLib)
import           Data.Text                               (Text, pack)
import           Data.Void                               (Void)
import           Text.Megaparsec                         (ParseError, ParseErrorBundle (ParseErrorBundle),
                                                          SourcePos (..), SourcePos, attachSourcePos, bundleErrors,
                                                          bundlePosState, eof, errorOffset, label, manyTill,
                                                          parseErrorPretty, runParser, sourceColumn, sourceLine, unPos)

processErrorBundle :: ParseErrorBundle Text Void -> [Text]
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors
  where
    parseErrorToGQLError :: (ParseError Text Void, SourcePos) -> Text
    parseErrorToGQLError (err, pos) = pack (parseErrorPretty err) <> toError pos
    -----------------------------------------------------------------------------------
    bundleToErrors :: ParseErrorBundle Text Void -> [(ParseError Text Void, SourcePos)]
    bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
      NonEmpty.toList $ fst $ attachSourcePos errorOffset bundleErrors bundlePosState
    --------------------------------------------
    toError :: SourcePos -> Text
    toError SourcePos {sourceLine, sourceColumn} =
      pack $ "Pos: " <> show (unPos sourceLine) <> ", " <> show (unPos sourceColumn)

parseDocument :: Text -> Either [Text] DataTypeLib
parseDocument doc =
  case parseDoc of
    Right root      -> Right root
    Left parseError -> Left $ processErrorBundle parseError
  where
    parseDoc = runParser request "<input>" doc
    request =
      label "Document" $ do
        ignoreComments
        dataTypes <- manyTill parseDataType eof
        buildLib dataTypes
      where
        buildLib types =
          case takeByKey "Query" types of
            (Just query, lib1) ->
              case takeByKey "Mutation" lib1 of
                (mutation, lib2) ->
                  case takeByKey "Subscription" lib2 of
                    (subscription, lib3) -> pure ((foldr defineType (initTypeLib query) lib3) {mutation, subscription})
            _ -> fail "Query Not Defined"
        ----------------------------------------------------------------------------
        takeByKey key lib =
          case lookup key lib of
            Just (OutputObject value) -> (Just (key, value), filter ((/= key) . fst) lib)
            _                         -> (Nothing, lib)
