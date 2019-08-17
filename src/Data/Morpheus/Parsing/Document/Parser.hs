{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.Parser
  ( parseDocument
  ) where

import           Data.Text                               (Text)
import           Text.Megaparsec                         (eof, label, manyTill, runParser)

-- MORPHEUS
import           Data.Morpheus.Parsing.Document.DataType (parseDataType)
import           Data.Morpheus.Parsing.Internal.Internal (processErrorBundle)
import           Data.Morpheus.Parsing.Internal.Terms    (spaceAndComments)
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataTypeLib (..), defineType, initTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)

parseDocument :: Text -> Validation DataTypeLib
parseDocument doc =
  case parseDoc of
    Right root      -> Right root
    Left parseError -> Left $ processErrorBundle parseError
  where
    parseDoc = runParser request "<input>" doc
    request =
      label "Document" $ do
        spaceAndComments
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
