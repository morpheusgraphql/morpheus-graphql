{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Parsing.Document.Parser
  ( parseTypes
  )
where

import           Data.Text                      ( Text )
import           Text.Megaparsec                ( eof
                                                , label
                                                , manyTill
                                                , runParser
                                                )

-- MORPHEUS
import           Data.Morpheus.Parsing.Document.TypeSystem
                                                ( parseDataType )
import           Data.Morpheus.Parsing.Internal.Internal
                                                ( processErrorBundle )
import           Data.Morpheus.Parsing.Internal.Terms
                                                ( spaceAndComments )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataType )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )

parseTypes :: Text -> Validation [(Text, DataType)]
parseTypes doc = case parseDoc of
  Right root       -> pure root
  Left  parseError -> failure (processErrorBundle parseError)
 where
  parseDoc = runParser request "<input>" doc
  request  = label "DocumentTypes" $ do
    spaceAndComments
    manyTill parseDataType eof
