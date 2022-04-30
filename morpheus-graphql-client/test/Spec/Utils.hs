{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Utils
  ( mockApi,
    defineClientWith,
    defineClientWithJSON,
    getFile,
    path,
  )
where

import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus.Client
  ( defineByDocumentFile,
    defineByIntrospectionFile,
  )
import Data.Semigroup ((<>))
import Data.Text
import Language.Haskell.TH
  ( Dec,
    Q,
  )
import Prelude
  ( FilePath,
    IO,
  )

path :: FilePath -> FilePath
path name = "test/Case/" <> name

getFile :: FilePath -> IO ByteString
getFile p = L.readFile (path p)

mockApi :: FilePath -> ByteString -> IO ByteString
mockApi p _ = getFile (p <> "/response.json")

relativePath :: FilePath -> Q FilePath
relativePath url = makeRelativeToProject (path url)

defineClientWith :: FilePath -> Text -> Q [Dec]
defineClientWith url exp = do
  p <- relativePath (url <> "/schema.gql")
  defineByDocumentFile p exp

defineClientWithJSON :: FilePath -> Text -> Q [Dec]
defineClientWithJSON url exp = do
  p <- relativePath (url <> "/schema.json")
  defineByIntrospectionFile p exp
