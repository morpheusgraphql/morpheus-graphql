{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Utils
  ( mockApi,
    schemaUrl,
    defineClientWith,
  )
where

import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor ((<$>))
import Data.Morpheus.Client
  ( defineByDocumentFile,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    GQLQuery,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
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
path (FieldName name) = "test/Case/" <> unpack name

schemaUrl :: FieldName -> FilePath
schemaUrl p = path p <> "/schema.gql"

withProject :: FilePath -> FilePath
withProject = ("morpheus-graphql-client/" <>)

mockApi :: FieldName -> ByteString -> IO ByteString
mockApi p _ = L.readFile (path p <> "/response.json")

fixFilePath :: FilePath -> Q FilePath
fixFilePath x = prefix <$> runIO (doesFileExist x)
  where
    prefix True = x
    prefix False = withProject x

defineClientWith ::
  FieldName ->
  (GQLQuery, String) ->
  Q [Dec]
defineClientWith url exp = do
  p <- fixFilePath (schemaUrl url)
  defineByDocumentFile p exp
