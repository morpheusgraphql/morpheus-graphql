{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client
  ( gql,
    Fetch (..),
    defineQuery,
    defineByDocument,
    defineByDocumentFile,
    defineByIntrospection,
    defineByIntrospectionFile,
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
import Data.Morpheus.Client.Build
  ( defineQuery,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.JSONSchema.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Core
  ( parseFullGQLDocument,
  )
import Data.Morpheus.Ext.Result
  ( Eventless,
  )
import Data.Morpheus.QuasiQuoter (gql)
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.Internal.AST
  ( GQLQuery,
    ScalarValue (..),
    Schema,
    VALID,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString)

defineByDocumentFile :: FilePath -> (GQLQuery, String) -> Q [Dec]
defineByDocumentFile filePath args = do
  qAddDependentFile filePath
  defineByDocument (L.readFile filePath) args

defineByIntrospectionFile :: FilePath -> (GQLQuery, String) -> Q [Dec]
defineByIntrospectionFile filePath args = do
  qAddDependentFile filePath
  defineByIntrospection (L.readFile filePath) args

defineByDocument :: IO ByteString -> (GQLQuery, String) -> Q [Dec]
defineByDocument doc = defineQuery (schemaByDocument doc)

schemaByDocument :: IO ByteString -> IO (Eventless (Schema VALID))
schemaByDocument documentGQL = parseFullGQLDocument <$> documentGQL

defineByIntrospection :: IO ByteString -> (GQLQuery, String) -> Q [Dec]
defineByIntrospection json = defineQuery (decodeIntrospection <$> json)
