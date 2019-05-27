{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server
  ( startWebSocket
  ) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ServerState
broadcast message clients = do
  forM_ clients sendMessage
  return clients
  where
    sendMessage (_, connection') = WS.sendTextData connection' message

startWebSocket :: IO ()
startWebSocket = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  connection' <- WS.acceptRequest pending
  WS.forkPingThread connection' 30
  msg <- WS.receiveData connection'
  initConnection connection' msg
  where
    initConnection connection' msg =
      flip finally disconnect $ do
        modifyMVar_ state $ \state' -> broadcast (fst client `mappend` " joined") (addClient client state')
        talk client state
      where
        client = (msg, connection')
        disconnect = do
          s <-
            modifyMVar state $ \s ->
              let s' = removeClient client s
               in return (s', s')
          broadcast (fst client `mappend` " disconnected") s

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state =
  forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)
