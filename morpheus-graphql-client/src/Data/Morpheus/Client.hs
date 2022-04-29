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
    ScalarValue (..),
    DecodeScalar (..),
    EncodeScalar (..),
    ID (..),
    declareGlobalTypes,
    declareLocalTypes,
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.Client.Declare
  ( declareGlobalTypes,
    declareLocalTypes,
    declareTypesIO,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.Internal.Types
  ( ExecutableClientDocument,
    FetchError (..),
    Mode (..),
    Source (..),
  )
import Data.Morpheus.QuasiQuoter (gql)
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.Internal.AST
  ( ScalarValue (..),
  )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString)

-- DEPRECATED: Legacy Code Exports

{-# DEPRECATED defineByDocumentFile' "use declareLocalTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByDocumentFile' :: Q FilePath -> ExecutableClientDocument -> Q [Dec]
defineByDocumentFile' qFilePath args = qFilePath >>= flip defineByDocumentFile args

{-# DEPRECATED defineByIntrospectionFile' "use declareLocalTypes" #-}

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByIntrospectionFile' :: Q FilePath -> ExecutableClientDocument -> Q [Dec]
defineByIntrospectionFile' path args = path >>= flip defineByIntrospectionFile args

-- with file

{-# DEPRECATED defineByIntrospectionFile "use declareLocalTypes" #-}
defineByIntrospectionFile :: FilePath -> ExecutableClientDocument -> Q [Dec]
defineByIntrospectionFile filePath args = do
  qAddDependentFile filePath
  defineByIntrospection (L.readFile filePath) args

{-# DEPRECATED defineByDocumentFile "use declareLocalTypes" #-}
defineByDocumentFile :: FilePath -> ExecutableClientDocument -> Q [Dec]
defineByDocumentFile filePath args = do
  qAddDependentFile filePath
  defineByDocument (L.readFile filePath) args

-- direct

{-# DEPRECATED defineByDocument "use declareLocalTypesIO" #-}
defineByDocument :: IO ByteString -> ExecutableClientDocument -> Q [Dec]
defineByDocument doc = declareTypesIO (GQL <$> doc) Legacy

{-# DEPRECATED defineByIntrospection "use declareLocalTypesIO" #-}
defineByIntrospection :: IO ByteString -> ExecutableClientDocument -> Q [Dec]
defineByIntrospection doc = declareTypesIO (JSON <$> doc) Legacy
