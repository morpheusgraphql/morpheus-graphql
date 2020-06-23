{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Scotty
  ( scottyServer,
  )
where

-- examples
import Client.Client
  ( fetchUser,
  )
import Control.Monad.IO.Class (liftIO)
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
      post "/" $ raw =<< (liftIO . httpPubApp api publish =<< body)
      addPlayground "/"
      get "/schema.gql" $ raw $ toGraphQLDocument $ Identity gqlRoot
      post "/mythology" $ raw =<< (liftIO . mythologyApi =<< body)
      addPlayground "/mythology"
      post "/th" $ raw =<< (liftIO . thSimpleApi =<< body)
      addPlayground "/th"
