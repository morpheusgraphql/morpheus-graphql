{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.ParseDocument
  ( parseGraphQLDocument
  , parseFullGQLDocument
  ) where

import           Control.Monad                           ((>=>))
import           Data.ByteString.Lazy.Char8              (ByteString)
import           Data.Text                               (Text)
import qualified Data.Text.Lazy                          as LT (toStrict)
import           Data.Text.Lazy.Encoding                 (decodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Document.Parsing.Parser   (parseDocument)
import           Data.Morpheus.Schema.SchemaAPI          (schemaTypes)
import           Data.Morpheus.Types.Internal.Data       (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)

parseGraphQLDocument :: ByteString -> Validation DataTypeLib
parseGraphQLDocument x = parseDocument (LT.toStrict $ decodeUtf8 x)

parseFullGQLDocument :: ByteString -> Validation DataTypeLib
parseFullGQLDocument = parseGraphQLDocument >=> schemaTypes
