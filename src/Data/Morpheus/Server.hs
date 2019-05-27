{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server
  ( socketApplication
  ) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Morpheus      (interpreter)
import           Data.Text          (Text, pack)
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

type ServerState = [Client]

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

joinClient :: Client -> ServerState -> IO ServerState
joinClient (id', connection') state' = broadcast (id' <> " joined") (addClient (id', connection') state')

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

disconnectClient :: Client -> MVar ServerState -> IO ServerState
disconnectClient client state = modifyMVar state removeUser >>= broadcast (fst client <> " disconnected")
  where
    removeUser state' =
      let s' = removeClient client state'
       in return (s', s')

generateID :: [a] -> Text
generateID = ("user" <>) . pack . show . length

broadcast :: Text -> ServerState -> IO ServerState
broadcast message clients = do
  forM_ clients sendMessage
  return clients
  where
    sendMessage (_, connection') = WS.sendTextData connection' message

socketApplication :: IO WS.ServerApp
socketApplication = do
  state <- newMVar []
  return (application state)

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  connection' <- WS.acceptRequest pending
  WS.forkPingThread connection' 30
  -- initialMessage <- WS.receiveData connection'
  id' <- generateID <$> readMVar state
  modifyMVar_ state $ joinClient (id', connection')
  initConnection (id', connection')
  where
    initConnection client' = finally (talk client' state) (disconnectClient client' state)

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state =
  forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast (user <> ": " <> msg)
