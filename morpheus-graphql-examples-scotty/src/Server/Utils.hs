{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Utils
  ( httpEndpoint,
  )
where

-- examples

import Client.Client
  ( fetchUser,
  )
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Morpheus.Document (toGraphQLDocument)
import Data.Morpheus.Server
  ( httpPubApp,
    webSocketsApp,
  )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import Network.WebSockets (defaultConnectionOptions)
import Server.Mythology.API (mythologyApi)
import Server.Playground (playground)
import Server.Sophisticated.API
  ( EVENT,
    api,
    gqlRoot,
  )
import Server.TH.Simple (thSimpleApi)
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

warpServer wsApp httpApp =
  Warp.runSettings settings $
    WaiWs.websocketsOr defaultConnectionOptions wsApp httpApp
  where
    settings = Warp.setPort 3000 Warp.defaultSettings
