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
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Functor.Identity (Identity (..))
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
  ( RoutePattern,
    ScottyM,
    body,
    get,
    post,
    raw,
    scottyApp,
  )

addPlayground :: RoutePattern -> ScottyM ()
addPlayground route = get route (raw playground)

httpEndpoint ::
  RoutePattern ->
  (ByteString -> IO ByteString) ->
  ScottyM ()
httpEndpoint route gqlApi = do
  addPlayground route
  post route $ raw =<< (liftIO . gqlApi =<< body)

scottyServer :: IO ()
scottyServer = do
  (wsApp, publish) <- webSocketsApp api
  httpApp <- httpServer publish
  fetchUser (httpPubApp api publish) >>= print
  Warp.runSettings settings $
    WaiWs.websocketsOr defaultConnectionOptions wsApp httpApp
  where
    settings = Warp.setPort 3000 Warp.defaultSettings
    httpServer :: (EVENT -> IO ()) -> IO Wai.Application
    httpServer publish = scottyApp $ do
      get "/schema" $ raw $ toGraphQLDocument $ Identity gqlRoot
      httpEndpoint "/" (httpPubApp api publish)
      httpEndpoint "/mythology" mythologyApi
      httpEndpoint "/th" thSimpleApi
