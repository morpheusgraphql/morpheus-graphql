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
  ( startServer,
    Endpoint,
    serveEndpoint,
    servePubEndpoint,
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
import Data.Morpheus (runApp)
import Data.Morpheus.Document
  ( RootResolverConstraint,
    toGraphQLDocument,
  )
import Data.Morpheus.Server
  ( httpPlayground,
    httpPubApp,
  )
import Data.Morpheus.Types
  ( App,
    GQLRequest,
    GQLResponse,
    RootResolver,
    render,
  )
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Network.Wai.Handler.WebSockets
  ( websocketsOr,
  )
import Network.WebSockets
  ( ServerApp,
    defaultConnectionOptions,
  )
import Servant
  ( (:<|>) (..),
    (:>),
    Accept (..),
    Get,
    HasServer,
    JSON,
    MimeRender (..),
    PlainText,
    Post,
    ReqBody,
    Server,
    serve,
  )

startServer ::
  HasServer api '[] =>
  ServerApp ->
  Proxy api ->
  Server api ->
  IO ()
startServer wsApp proxy api =
  runSettings settings $
    websocketsOr
      defaultConnectionOptions
      wsApp
      (serve proxy api)
  where
    settings = setPort 3000 defaultSettings

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type API = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

type Schema = "schema" :> Get '[PlainText] Text

type Playground = Get '[HTML] ByteString

type Endpoint (name :: Symbol) = name :> (API :<|> Schema :<|> Playground)

serveEndpoint :: App e IO -> Server (Endpoint name)
serveEndpoint app = (liftIO . runApp app) :<|> withSchema app :<|> pure httpPlayground

servePubEndpoint :: App e IO -> (e -> IO ()) -> Server (Endpoint name)
servePubEndpoint app publish = (liftIO . httpPubApp app publish) :<|> withSchema app :<|> pure httpPlayground

withSchema :: (Applicative f) => App e m -> f Text
withSchema = pure . render
