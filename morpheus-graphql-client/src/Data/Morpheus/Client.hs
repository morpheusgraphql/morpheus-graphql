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
    Request (..),
    request,
    forEach,
    single,
    ClientStream,
  )
where

import Data.Aeson
import qualified Data.Aeson as A
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
    decodeResponse,
    processResponse,
    toRequest,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientResult,
    ExecutableSource,
    FetchError (..),
    SchemaSource (..),
  )
import Data.Morpheus.Client.Schema.JSON.Types
import Data.Morpheus.Subscriptions.Internal (ApolloSubscription (..))
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
  ( Option,
    POST (..),
    ReqBodyLbs (..),
    defaultHttpConfig,
    header,
    lbsResponse,
    req,
    responseBody,
    runReq,
    useURI,
  )
import Network.WebSockets (ClientApp, receiveData, runClient, sendTextData)
import Relude hiding (ByteString)
import Text.URI (Authority (..), RText, RTextLabel (PathPiece), URI (..), mkURI, unRText)

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

headers :: Network.HTTP.Req.Option scheme
headers = header "Content-Type" "application/json"

post :: URI -> ByteString -> IO ByteString
post uri body = case useURI uri of
  Nothing -> fail ("Invalid Endpoint: " <> show uri <> "!")
  (Just (Left (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> headers))
  (Just (Right (u, o))) -> responseBody <$> runReq defaultHttpConfig (req POST u (ReqBodyLbs body) lbsResponse (o <> headers))

parseURI :: MonadFail m => String -> m URI
parseURI url = maybe (fail ("Invalid Endpoint: " <> show url <> "!")) pure (mkURI (T.pack url))

toPort :: Maybe Word -> Int
toPort Nothing = 80
toPort (Just x) = fromIntegral x

getPath :: Maybe (Bool, NonEmpty (RText 'PathPiece)) -> String
getPath (Just (_, h :| t)) = T.unpack $ T.intercalate "/" $ fmap unRText (h : t)
getPath _ = ""

useWS :: URI -> ClientApp () -> IO ()
useWS URI {uriScheme = Just scheme, uriAuthority = Right Authority {authHost, authPort}, uriPath} app
  | unRText scheme == "ws" = do
    let rPath = getPath uriPath
    let rPort = toPort authPort
    let rHost = handleHost $ unRText authHost
    runClient rHost rPort rPath app
useWS uri _ = fail ("Invalid Endpoint: " <> show uri <> "!")

handleHost :: Text -> String
handleHost "localhost" = "127.0.0.1"
handleHost x = T.unpack x

requestSingle :: Fetch a => URI -> Request method a -> IO (Either (FetchError a) a)
requestSingle uri r = decodeResponse <$> post uri (A.encode $ toRequest r)

requestStream :: (Fetch a) => URI -> Request method a -> (ClientResult a -> IO ()) -> IO ()
requestStream uri r f = useWS uri app
  where
    decodeMessage = (first FetchErrorParseFailure . eitherDecode) >=> processMessage
    processMessage :: ApolloSubscription (JSONResponse a) -> Either (FetchError a) a
    processMessage ApolloSubscription {apolloPayload = Just payload} = processResponse payload
    processMessage ApolloSubscription {} = Left (FetchErrorParseFailure "empty message")
    app conn = do
      sendTextData conn (A.encode initMessage)
      -- send initial GQL Request for subscription
      sendTextData
        conn
        ( A.encode
            ApolloSubscription
              { apolloPayload = Just (toRequest r),
                apolloType = "start",
                apolloId = Just "01"
              }
        )
      -- handle GQL subscription responses
      forever $ do
        message <- receiveData conn
        f (decodeMessage message)

initMessage :: ApolloSubscription ()
initMessage = ApolloSubscription {apolloType = "connection_init", apolloPayload = Nothing, apolloId = Nothing}

-- endMessage :: Text -> ApolloSubscription ()
-- endMessage uid = ApolloSubscription {apolloType = "stop", apolloPayload = Nothing, apolloId = Just uid}

data ClientStream t a = Fetch a =>
  ClientStream
  { _req :: Request t a,
    _uri :: URI
  }

request :: Fetch a => String -> Args a -> IO (ClientStream t a)
request uri requestArgs = do
  _uri <- parseURI uri
  let _req = Request {requestArgs}
  pure ClientStream {_req, _uri}

forEach :: (ClientResult a -> IO ()) -> ClientStream method a -> IO ()
forEach f ClientStream {_uri, _req} = requestStream _uri _req f

single :: ClientStream method a -> IO (ClientResult a)
single ClientStream {_req, _uri} = requestSingle _uri _req