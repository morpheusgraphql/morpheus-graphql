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
import Server.Sophisticated.API
  ( EVENT,
    api,
    gqlRoot,
  )
import Server.TH.Simple (thSimpleApi)
import Server.Utils
  ( httpEndpoint,
    warpServer,
  )
import Web.Scotty
  ( get,
    raw,
    scottyApp,
  )

scottyServer :: IO ()
scottyServer = do
  (wsApp, publish) <- webSocketsApp api
  httpApp <- httpServer publish
  fetchUser (httpPubApp api publish) >>= print
  warpServer wsApp httpApp
  where
    httpServer :: (EVENT -> IO ()) -> IO Wai.Application
    httpServer publish = scottyApp $ do
      httpEndpoint "/" (Just $ toGraphQLDocument $ Identity gqlRoot) (httpPubApp api publish)
      httpEndpoint "/mythology" Nothing mythologyApi
      httpEndpoint "/th" Nothing thSimpleApi
