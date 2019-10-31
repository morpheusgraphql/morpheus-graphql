{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.Parser
  ( parseTypes
  ) where

import           Data.Text                                 (Text)
import           Text.Megaparsec                           (eof, label, manyTill, runParser)

-- MORPHEUS
import           Data.Morpheus.Parsing.Document.TypeSystem (parseDataType)
import           Data.Morpheus.Parsing.Internal.Internal   (processErrorBundle)
import           Data.Morpheus.Parsing.Internal.Terms      (spaceAndComments)
import           Data.Morpheus.Types.Internal.Data         (RawDataType)
import           Data.Morpheus.Types.Internal.Validation   (Validation)

parseTypes :: Text -> Validation [(Text, RawDataType)]
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
