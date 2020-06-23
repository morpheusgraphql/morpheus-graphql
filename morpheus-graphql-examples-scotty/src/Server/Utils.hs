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
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Document
  ( RootResolverConstraint,
    toGraphQLDocument,
  )
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Types
  ( RootResolver,
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
  (RootResolverConstraint m o mu qu su) =>
  RoutePattern ->
  RootResolver m o mu qu su ->
  (ByteString -> IO ByteString) ->
  ScottyM ()
httpEndpoint route schema gqlApi = do
  get route $
    ( do
        _ <- isSchema
        raw $ toGraphQLDocument (Identity schema)
    )
      <|> raw httpPlayground
  post route $ raw =<< (liftIO . gqlApi =<< body)

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
