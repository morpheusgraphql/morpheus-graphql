{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    request,
  )
where

import Control.Concurrent
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
    Request (..),
    Response,
    decodeResponse,
    encodeRequest,
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
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Network.HTTP.Req
import Network.WebSockets (receiveData, runClient, sendTextData)
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

request :: Fetch a => Request method IO a -> Response method IO a
request r@HttpRequest {httpEndpoint} = runReq defaultHttpConfig $ do
  let headers = header "Content-Type" "application/json"
  decodeResponse . responseBody
    <$> req
      POST
      (https httpEndpoint)
      (ReqBodyLbs (encodeRequest r))
      lbsResponse
      headers
request r@WSSubscription {subscriptionHandler, wsEndpoint} =
  runClient
    (T.unpack wsEndpoint)
    0
    ""
    ( \conn -> do
        -- send initial GQL Request for subscription
        sendTextData conn (encodeRequest r)
        -- handle GQL subscription responses
        void . forkIO . forever $ do
          message <- receiveData conn
          subscriptionHandler (decodeResponse message)
    )
