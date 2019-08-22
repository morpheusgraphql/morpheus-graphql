{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.Parser
  ( parseDocument
  , parseTypes
  ) where

import           Data.Text                               (Text)
import           Text.Megaparsec                         (eof, label, manyTill, runParser)

-- MORPHEUS
import           Data.Morpheus.Parsing.Document.DataType (parseDataType)
import           Data.Morpheus.Parsing.Internal.Create   (createDataTypeLib)
import           Data.Morpheus.Parsing.Internal.Internal (processErrorBundle)
import           Data.Morpheus.Parsing.Internal.Terms    (spaceAndComments)
import           Data.Morpheus.Types.Internal.Data       (DataFullType, DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)

parseTypes :: Text -> Validation [(Text, DataFullType)]
parseTypes doc =
  case parseDoc of
    Right root      -> Right root
    Left parseError -> Left $ processErrorBundle parseError
  where
    parseDoc = runParser request "<input>" doc
    request =
      label "DocumentTypes" $ do
        spaceAndComments
        manyTill parseDataType eof

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
        createDataTypeLib dataTypes
