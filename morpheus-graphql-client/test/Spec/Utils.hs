{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Utils
  ( mockApi,
    defineClientWith,
    defineClientWithJSON,
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
  ( FieldName (..),
    GQLQuery,
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
path (FieldName name) = "test/Case/" <> T.unpack name

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
  p <- fixFilePath (path url <> "/schema.gql")
  defineByDocumentFile p exp

defineClientWithJSON ::
  FieldName ->
  (GQLQuery, String) ->
  Q [Dec]
defineClientWithJSON url exp = do
  p <- fixFilePath (path url <> "/schema.json")
  defineByIntrospectionFile p exp
