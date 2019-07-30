{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Data.Morpheus.Document.ParseDocument
  ( parseGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8            (ByteString)
import           Data.Text                             (Text)
import qualified Data.Text.Lazy                        as LT (toStrict)
import           Data.Text.Lazy.Encoding               (decodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Document.Parsing.Parser (parseDocument)
import           Data.Morpheus.Types.Internal.Data     (DataTypeLib)

parseGraphQLDocument :: ByteString -> Either [Text] DataTypeLib
parseGraphQLDocument x = parseDocument (LT.toStrict $ decodeUtf8 x)
