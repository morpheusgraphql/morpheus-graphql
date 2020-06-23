{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Utils
  ( --startServer,
    Endpoint,
    serveEndpoint,
  )
where

-- examples
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString,
    unpack,
  )
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Morpheus.Document
  ( RootResolverConstraint,
    toGraphQLDocument,
  )
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    RootResolver,
  )
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
-- import Network.Wai.Handler.Warp
--   ( defaultSettings,
--     runSettings,
--     setPort,
--   )
-- import Network.Wai.Handler.WebSockets
--   ( websocketsOr,
--   )
-- import Network.WebSockets
--   ( ServerApp,
--     defaultConnectionOptions,
--   )
import Servant
  ( (:<|>) (..),
    (:>),
    Accept (..),
    Get,
    JSON,
    MimeRender (..),
    PlainText,
    Post,
    QueryParam',
    ReqBody,
    Server,
    Strict,
  )

-- TODO:
-- startServer :: ServerApp -> ScottyM () -> IO ()
-- startServer wsApp app = do
--   httpApp <- scottyApp app
--   runSettings settings $
--     websocketsOr
--       defaultConnectionOptions
--       wsApp
--       httpApp
--   where
--     settings = setPort 3000 defaultSettings

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type API = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

type Schema = "schema" :> Get '[PlainText] Text

type Playground = Get '[HTML] ByteString

type Endpoint (name :: Symbol) = name :> (API :<|> Schema :<|> Playground)

serveEndpoint ::
  (RootResolverConstraint m o mu qu su) =>
  RootResolver m o mu qu su ->
  (GQLRequest -> IO GQLResponse) ->
  Server (Endpoint name)
serveEndpoint root app = (liftIO . app) :<|> withSchema root :<|> pure httpPlayground

withSchema ::
  (RootResolverConstraint m o mu qu su, Applicative f) =>
  RootResolver m o mu qu su ->
  f Text
withSchema schema = pure $ pack $ unpack $ toGraphQLDocument (Identity schema)
