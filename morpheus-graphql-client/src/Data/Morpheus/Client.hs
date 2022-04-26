{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client
  ( gql,
    Fetch (..),
    FetchError (..),
    defineQuery,
    defineByDocument,
    defineByDocumentFile,
    defineByDocumentFile',
    defineByIntrospection,
    defineByIntrospectionFile,
    defineByIntrospectionFile',
    declareLocalTypes,
    declareGlobalTypes,
    ScalarValue (..),
    DecodeScalar (..),
    EncodeScalar (..),
    ID (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.List (isSuffixOf)
import Data.Morpheus.Client.Build
  ( defineQuery, defineGlobalTypes,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.Internal.Types
  ( FetchError (..),
    Mode (..),
  )
import Data.Morpheus.Client.JSONSchema.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Core
  ( parseFullSchema,
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.QuasiQuoter (gql)
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.Internal.AST
  ( ExecutableDocument,
    ScalarValue (..),
    Schema,
    VALID,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString)

defineByDocumentFile :: FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByDocumentFile filePath args = do
  qAddDependentFile filePath
  defineByDocument (L.readFile filePath) args

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByDocumentFile' :: Q FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByDocumentFile' qFilePath args = qFilePath >>= flip defineByDocumentFile args

defineByIntrospectionFile :: FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByIntrospectionFile filePath args = do
  qAddDependentFile filePath
  defineByIntrospection (L.readFile filePath) args

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByIntrospectionFile' :: Q FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByIntrospectionFile' qFilePath args = qFilePath >>= flip defineByIntrospectionFile args

defineByDocument :: IO ByteString -> (ExecutableDocument, String) -> Q [Dec]
defineByDocument doc = defineQuery Both (fmap parseFullSchema doc)

defineByIntrospection :: IO ByteString -> (ExecutableDocument, String) -> Q [Dec]
defineByIntrospection json = defineQuery Both (decodeIntrospection <$> json)

readSchema :: FilePath -> IO (GQLResult (Schema VALID))
readSchema path
  | ".json" `isSuffixOf` path = decodeIntrospection <$> L.readFile path
  | ".gql" `isSuffixOf` path || ".graphql" `isSuffixOf` path = parseFullSchema <$> L.readFile path
  | otherwise = fail "unsupported file format!"

declareLocalTypes :: Q FilePath -> (ExecutableDocument, String) -> Q [Dec]
declareLocalTypes qPath doc = do
  p <- qPath
  defineQuery Local (readSchema p) doc

declareGlobalTypes :: Q FilePath -> Q [Dec]
declareGlobalTypes qPath = do
  p <- qPath
  defineGlobalTypes (readSchema p)
