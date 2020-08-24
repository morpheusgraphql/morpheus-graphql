{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Utils
  ( httpEndpoint,
    startServer,
  )
where

-- examples
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Morpheus.Server
  ( httpPlayground,
    httpPubApp,
  )
import Data.Morpheus.Types
  ( App,
    render,
  )
import qualified Data.Text as T
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
import Web.Scotty
  ( ActionM,
    RoutePattern,
    ScottyM,
    body,
    get,
    param,
    post,
    raw,
    scottyApp,
  )

isSchema :: ActionM String
isSchema = param "schema"

httpEndpoint ::
  RoutePattern ->
  [e -> IO ()] ->
  App e IO ->
  ScottyM ()
httpEndpoint route publish app = do
  get route $
    ( do
        _ <- isSchema
        raw $ LBS.pack $ T.unpack $ render app
    )
      <|> raw httpPlayground
  post route $ raw =<< (liftIO . httpPubApp publish app =<< body)

startServer :: ServerApp -> ScottyM () -> IO ()
startServer wsApp app = do
  httpApp <- scottyApp app
  runSettings settings $
    websocketsOr
      defaultConnectionOptions
      wsApp
      httpApp
  where
    settings = setPort 3000 defaultSettings
