{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( mockApi,
    schemaUrl,
  )
where

import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Types.Internal.AST (FieldName (..))
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Prelude
  ( FilePath,
    IO,
  )

path :: FieldName -> FilePath
path (FieldName name) = "test/Case/" <> unpack name

schemaUrl :: FieldName -> FilePath
schemaUrl p = path p <> "/schema.gql"

mockApi :: FieldName -> ByteString -> IO ByteString
mockApi p _ = L.readFile (path p <> "/response.json")
