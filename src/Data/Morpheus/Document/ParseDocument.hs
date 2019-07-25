{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.ParseDocument
  ( parseGraphQLDocument
  ) where

import           Data.Text                             (Text)

-- MORPHEUS
import           Data.Morpheus.Document.Parsing.Parser (parseDocument)
import           Data.Morpheus.Types.Internal.Data     (DataTypeLib)

parseGraphQLDocument :: Either [Text] DataTypeLib
parseGraphQLDocument = parseDocument document
  where
    document = "type Query {\n  deity : Deity \n} \n type Deity {\n  fullName: String!\n  power: String\n}"
