{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client
  ( gql,
    Fetch (..),
    FetchError (..),
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
  ( defineGlobalTypes,
    defineQuery,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.Internal.Types
  ( FetchError (..),
    Mode (..),
    Source (..),
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

type ExecutableDoc = (ExecutableDocument, String)

{-# DEPRECATED defineByDocumentFile "use declareLocalTypes" #-}
defineByDocumentFile :: FilePath -> ExecutableDoc -> Q [Dec]
defineByDocumentFile filePath args = do
  qAddDependentFile filePath
  defineByDocument (L.readFile filePath) args

{-# DEPRECATED defineByDocumentFile' "use declareLocalTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByDocumentFile' :: Q FilePath -> ExecutableDoc -> Q [Dec]
defineByDocumentFile' qFilePath args = qFilePath >>= flip defineByDocumentFile args

{-# DEPRECATED defineByIntrospectionFile "use declareLocalTypes" #-}
defineByIntrospectionFile :: FilePath -> ExecutableDoc -> Q [Dec]
defineByIntrospectionFile filePath args = do
  qAddDependentFile filePath
  defineByIntrospection (L.readFile filePath) args

{-# DEPRECATED defineByIntrospectionFile' "use declareLocalTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByIntrospectionFile' :: Q FilePath -> ExecutableDoc -> Q [Dec]
defineByIntrospectionFile' qFilePath args = qFilePath >>= flip defineByIntrospectionFile args

{-# DEPRECATED defineByDocument "use declareLocalTypesIO" #-}

defineByDocument :: IO ByteString -> ExecutableDoc -> Q [Dec]
defineByDocument doc = declareTypesIO  (GQL <$> doc) Both

{-# DEPRECATED defineByIntrospection "use declareLocalTypesIO" #-}

defineByIntrospection :: IO ByteString -> ExecutableDoc -> Q [Dec]
defineByIntrospection doc = declareTypesIO (JSON <$> doc) Both

parseSchema :: Source -> GQLResult (Schema VALID)
parseSchema (JSON doc) = decodeIntrospection doc
parseSchema (GQL doc) = parseFullSchema doc

declareTypesIO :: IO Source -> Mode -> ExecutableDoc -> Q [Dec]
declareTypesIO doc mode = defineQuery mode (parseSchema <$> doc)

declareLocalTypesIO :: IO Source -> ExecutableDoc -> Q [Dec]
declareLocalTypesIO doc = declareTypesIO doc Local

declareGlobalTypesIO :: IO Source -> Q [Dec]
declareGlobalTypesIO doc = defineGlobalTypes (parseSchema <$> doc)

parseSource :: FilePath -> IO Source
parseSource p
  | ".json" `isSuffixOf` p = JSON <$> L.readFile p
  | ".gql" `isSuffixOf` p || ".graphql" `isSuffixOf` p = GQL <$> L.readFile p
  | otherwise = fail "unsupported file format!"

declareLocalTypes :: Q FilePath -> ExecutableDoc -> Q [Dec]
declareLocalTypes qPath doc = do
  p <- qPath
  declareTypesIO (parseSource p) Local doc

declareGlobalTypes :: Q FilePath -> Q [Dec]
declareGlobalTypes qPath = do
  p <- qPath
  declareGlobalTypesIO (parseSource p)
