{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Utils
  ( mockApi,
    defineClientWith,
    defineClientWithJSON,
    fixedSchemaPath,
    getFile,
  )
where

import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor ((<$>))
import Data.Morpheus.Client
  ( defineByDocumentFile,
    defineByIntrospectionFile,
  )
import Data.Morpheus.Types.Internal.AST
  ( ExecutableDocument,
    FieldName,
    unpackName,
  )
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Language.Haskell.TH
  ( Dec,
    Q,
    runIO,
  )
import System.Directory (doesFileExist)
import Prelude
  ( Bool (..),
    FilePath,
    IO,
    String,
  )

path :: FieldName -> FilePath
path name = "test/Case/" <> T.unpack (unpackName name)

withProject :: FilePath -> FilePath
withProject = ("morpheus-graphql-client/" <>)

getFile :: FieldName -> IO ByteString
getFile p = L.readFile (path p)

mockApi :: FieldName -> ByteString -> IO ByteString
mockApi p _ = getFile (p <> "/response.json")

fixFilePath :: FilePath -> Q FilePath
fixFilePath x = prefix <$> runIO (doesFileExist x)
  where
    prefix True = x
    prefix False = withProject x

fixedSchemaPath :: FieldName -> Q FilePath
fixedSchemaPath url = fixFilePath (path url <> "/schema.gql")

defineClientWith ::
  FieldName ->
  (ExecutableDocument, String) ->
  Q [Dec]
defineClientWith url exp = do
  p <- fixedSchemaPath url
  defineByDocumentFile p exp

defineClientWithJSON ::
  FieldName ->
  (ExecutableDocument, String) ->
  Q [Dec]
defineClientWithJSON url exp = do
  p <- fixFilePath (path url <> "/schema.json")
  defineByIntrospectionFile p exp
