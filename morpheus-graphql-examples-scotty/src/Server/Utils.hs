{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Utils
  ( httpEndpoint,
    warpServer,
  )
where

-- examples
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Morpheus.Document (toGraphQLDocument)
import Network.Wai
  ( Application,
  )
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
import Server.Playground (playground)
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
  Maybe ByteString ->
  (ByteString -> IO ByteString) ->
  ScottyM ()
httpEndpoint route schema gqlApi = do
  get route $
    ( do
        x <- isSchema
        raw $ fromMaybe "# schema is not provided" schema
    )
      <|> raw playground
  post route $ raw =<< (liftIO . gqlApi =<< body)

warpServer :: ServerApp -> ScottyM () -> IO ()
warpServer wsApp app = do
  httpApp <- scottyApp app
  runSettings settings $
    websocketsOr
      defaultConnectionOptions
      wsApp
      httpApp
  where
    settings = setPort 3000 defaultSettings
