{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.Utils
  ( runServer,
    Endpoint,
    serveEndpoint,
    ServantAPI (..),
  )
where

-- examples
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Subscriptions
  ( PubApp,
    SubApp,
    httpPubApp,
  )
import Data.Morpheus.Types
  ( App,
    GQLRequest,
    GQLResponse,
    render,
  )
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
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
import Prelude

class ServerRunner a where
  runServer :: ServerApp -> a -> IO ()

class HttpEndpoint e m server where
  httpEndpoint :: [e -> m ()] -> App e m -> server

newtype ServantAPI api = ServantAPI {unS :: (Proxy api, Server api)}

newtype Path name = Path {_unPath :: Server (Endpoint name)}

instance PubApp e => HttpEndpoint e IO (Path name) where
  httpEndpoint publish app =
    Path $
      (liftIO . httpPubApp publish app) :<|> withSchema app :<|> pure httpPlayground

serveEndpoint ::
  ( SubApp ServerApp e,
    PubApp e
  ) =>
  [e -> IO ()] ->
  App e IO ->
  Server (Endpoint name)
serveEndpoint events = _unPath . httpEndpoint events

instance HasServer api '[] => ServerRunner (ServantAPI api) where
  runServer wsApp ServantAPI {unS = (proxy, api)} =
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

withSchema :: (Applicative f) => App e m -> f Text
withSchema = pure . LT.toStrict . decodeUtf8 . render
