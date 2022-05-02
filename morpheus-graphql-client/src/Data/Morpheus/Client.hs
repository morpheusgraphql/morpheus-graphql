{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client
  ( raw,
    Fetch (..),
    FetchError (..),
    ScalarValue (..),
    DecodeScalar (..),
    EncodeScalar (..),
    ID (..),
    declareGlobalTypes,
    declareGlobalTypesByName,
    declareLocalTypes,
    declareLocalTypesInline,
    clientTypeDeclarations,
    -- DEPRECATED EXPORTS
    gql,
    defineByDocument,
    defineByDocumentFile,
    defineByDocumentFile',
    defineByIntrospection,
    defineByIntrospectionFile,
    defineByIntrospectionFile',
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.Client.Declare
  ( clientTypeDeclarations,
    declareGlobalTypes,
    declareGlobalTypesByName,
    declareLocalTypes,
    declareLocalTypesInline,
    internalLegacyLocalDeclareTypes,
    raw,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.Internal.Types
  ( ExecutableSource,
    FetchError (..),
    SchemaSource (..),
  )
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

{-# DEPRECATED defineByDocumentFile' "use declareLocalTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByDocumentFile' :: Q FilePath -> ExecutableSource -> Q [Dec]
defineByDocumentFile' qFilePath args = qFilePath >>= flip defineByDocumentFile args

{-# DEPRECATED defineByIntrospectionFile' "use declareLocalTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByIntrospectionFile' :: Q FilePath -> ExecutableSource -> Q [Dec]
defineByIntrospectionFile' path args = path >>= flip defineByIntrospectionFile args

-- with file

{-# DEPRECATED defineByIntrospectionFile "use declareLocalTypes" #-}
defineByIntrospectionFile :: FilePath -> ExecutableSource -> Q [Dec]
defineByIntrospectionFile filePath args = do
  qAddDependentFile filePath
  defineByIntrospection (L.readFile filePath) args

{-# DEPRECATED defineByDocumentFile "use declareLocalTypes" #-}
defineByDocumentFile :: FilePath -> ExecutableSource -> Q [Dec]
defineByDocumentFile filePath args = do
  qAddDependentFile filePath
  defineByDocument (L.readFile filePath) args

-- direct

{-# DEPRECATED defineByDocument "use clientTypeDeclarations" #-}
defineByDocument :: IO ByteString -> ExecutableSource -> Q [Dec]
defineByDocument doc = internalLegacyLocalDeclareTypes (GQL <$> doc)

{-# DEPRECATED defineByIntrospection "use clientTypeDeclarations" #-}
defineByIntrospection :: IO ByteString -> ExecutableSource -> Q [Dec]
defineByIntrospection doc = internalLegacyLocalDeclareTypes (JSON <$> doc)
