{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client
  ( gql,
    raw,
    Fetch (..),
    FetchError (..),
    defineByDocument,
    defineByDocumentFile,
    defineByDocumentFile',
    defineByIntrospection,
    defineByIntrospectionFile,
    defineByIntrospectionFile',
    ScalarValue (..),
    DecodeScalar (..),
    EncodeScalar (..),
    ID (..),
    declareClientTypes,
    declareClientTypesInline,
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.Client.Declare
  ( declareClientTypes,
    declareClientTypesInline,
    declareTypesLegacy,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.Internal.Types
  ( ExecutableSource,
    FetchError (..),
    Mode (..),
    SchemaSource (..),
  )
import Data.Morpheus.Client.QuasiQuoter (raw)
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.Internal.AST
  ( ScalarValue (..),
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString)

{-# DEPRECATED gql "use raw" #-}
gql :: QuasiQuoter
gql = raw

-- DEPRECATED: Legacy Code Exports

{-# DEPRECATED defineByDocumentFile' "use declareClientTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByDocumentFile' :: Q FilePath -> ExecutableSource -> Q [Dec]
defineByDocumentFile' qFilePath args = qFilePath >>= flip defineByDocumentFile args

{-# DEPRECATED defineByIntrospectionFile' "use declareClientTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByIntrospectionFile' :: Q FilePath -> ExecutableSource -> Q [Dec]
defineByIntrospectionFile' path args = path >>= flip defineByIntrospectionFile args

-- with file

{-# DEPRECATED defineByIntrospectionFile "use declareClientTypes" #-}
defineByIntrospectionFile :: FilePath -> ExecutableSource -> Q [Dec]
defineByIntrospectionFile filePath args = do
  qAddDependentFile filePath
  defineByIntrospection (L.readFile filePath) args

{-# DEPRECATED defineByDocumentFile "use declareClientTypes" #-}
defineByDocumentFile :: FilePath -> ExecutableSource -> Q [Dec]
defineByDocumentFile filePath args = do
  qAddDependentFile filePath
  defineByDocument (L.readFile filePath) args

-- direct

{-# DEPRECATED defineByDocument "use declareClientTypesIO" #-}
defineByDocument :: IO ByteString -> ExecutableSource -> Q [Dec]
defineByDocument doc = declareTypesLegacy (GQL <$> doc) Legacy

{-# DEPRECATED defineByIntrospection "use declareClientTypesIO" #-}
defineByIntrospection :: IO ByteString -> ExecutableSource -> Q [Dec]
defineByIntrospection doc = declareTypesLegacy (JSON <$> doc) Legacy
