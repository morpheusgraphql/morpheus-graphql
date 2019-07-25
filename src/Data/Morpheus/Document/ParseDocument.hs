{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.RenderGraphQL
  ( renderGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text, intercalate)
import qualified Data.Text.Lazy             as LT (fromStrict)
import           Data.Text.Lazy.Encoding    (encodeUtf8)

-- MORPHEUS
--import           Data.Morpheus.Types.Internal.Data (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
--                                                    DataType (..), DataTypeLib, allDataTypes, showWrappedType)
renderGraphQLDocument :: ByteString
renderGraphQLDocument = document
  where
    document =
      "type Query {\n  deity(name: String!, mythology: Text): Deity\n} deriving (Generic)\n\ntype Deity {\n  fullName: String!\n  power : String\n}"
