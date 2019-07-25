{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.ParseDocument
  ( renderGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8            (ByteString, pack)

-- MORPHEUS
import           Data.Morpheus.Document.Parsing.Parser (parseDocument)

renderGraphQLDocument :: ByteString
renderGraphQLDocument = pack $ show (parseDocument document)
  where
    document =
      "type Query {\n  deity(name: String!, mythology: Text): Deity\n} deriving (Generic)\n\ntype Deity {\n  fullName: String!\n  power : String\n}"
