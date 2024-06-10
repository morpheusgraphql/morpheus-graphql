{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Server (main) where

import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.List
import qualified Data.Maybe as Maybe
import qualified Data.Morpheus (deriveApp)
import qualified Data.Morpheus.Subscriptions as MorpheusSub
import Data.Morpheus.Types as MT
  ( GQLRequest,
    GQLResponse,
    Undefined (..),
  )
import qualified Data.Text as T
import GHC.Generics
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai.Handler.WebSockets as WaiWebSockets
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.WebSockets.Connection as WebSockets
import qualified Server.Gql as Gql
import Server.ServerState (ServerState)
import qualified Server.ServerState as ServerState
import Yesod

mkYesod
  "ServerState"
  [parseRoutes|
/api GraphqlR POST
|]

instance Yesod ServerState

postGraphqlR :: Handler Value
postGraphqlR = do
  serverState <- getYesod
  let morpheusApp = Data.Morpheus.deriveApp $ Gql.rootResolver serverState
  body <- requireCheckJsonBody :: Handler GQLRequest
  publisher <- liftIO $ ServerState.readPublisher serverState
  result <-
    liftIO
      (MorpheusSub.httpPubApp [publisher] morpheusApp body :: IO GQLResponse)
  returnJson result

main :: IO ()
main = do
  serverState <- ServerState.new
  waiApp <- mkApp serverState >>= mkWebsocketApp serverState
  -- warpDebug 3000 $ waiApp
  Warp.run 3000 waiApp

mkApp :: ServerState -> IO Application
mkApp serverState = do
  waiApp <- Yesod.toWaiAppPlain serverState
  return $ Yesod.defaultMiddlewaresNoLogging $ corsPolicy waiApp

mkWebsocketApp :: ServerState -> Application -> IO Application
mkWebsocketApp serverState httpApp = do
  let morpheusApp = Data.Morpheus.deriveApp $ Gql.rootResolver serverState
  (wsApp, pub) <- MorpheusSub.webSocketsApp morpheusApp
  ServerState.savePublisher serverState pub
  return
    $ WaiWebSockets.websocketsOr
      WebSockets.defaultConnectionOptions
      wsApp
      httpApp

corsPolicy :: Wai.Middleware
corsPolicy = Cors.cors (const $ Just corsResourcePolicy)
  where
    corsResourcePolicy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsMethods = ["GET", "HEAD", "POST", "PATCH", "DELETE", "OPTION"],
          Cors.corsRequestHeaders = Cors.simpleHeaders <> ["Content-Type"]
        }
