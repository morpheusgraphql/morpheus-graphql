{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Utils
  ( mockApi,
    defineClientWith,
    defineClientWithJSON,
    getFile,
    relativePath,
  )
where

import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus.Client
  ( defineByDocumentFile,
    defineByIntrospectionFile,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    unpackName,
  )
import Data.Semigroup ((<>))
import Data.Text
import qualified Data.Text as T
import Language.Haskell.TH
  ( Dec,
    Q,
  )
import Prelude
  ( FilePath,
    IO,
  )

path :: FieldName -> FilePath
path name = "test/Case/" <> T.unpack (unpackName name)

getFile :: FieldName -> IO ByteString
getFile p = L.readFile (path p)

mockApi :: FieldName -> ByteString -> IO ByteString
mockApi p _ = getFile (p <> "/response.json")

relativePath :: FieldName -> Q FilePath
relativePath url = makeRelativeToProject (path url)

defineClientWith :: FieldName -> Text -> Q [Dec]
defineClientWith url exp = do
  p <- relativePath (url <> "/schema.gql")
  defineByDocumentFile p exp

defineClientWithJSON :: FieldName -> Text -> Q [Dec]
defineClientWithJSON url exp = do
  p <- relativePath (url <> "/schema.json")
  defineByIntrospectionFile p exp
