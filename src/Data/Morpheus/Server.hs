{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server
  ( socketApplication
  ) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)

--import           Data.Morpheus      (GQLRootResolver, interpreter)
import           Data.Text          (Text, pack)
import           Network.WebSockets (Connection, ServerApp, acceptRequest, forkPingThread, receiveData, sendTextData)

type Client = (Text, Connection)

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
generateID = ("connection_" <>) . pack . show . length

broadcast :: Text -> ServerState -> IO ServerState
broadcast message clients = do
  forM_ clients sendMessage
  return clients
  where
    sendMessage (_, connection') = sendTextData connection' message

socketApplication :: (Text -> IO Text) -> IO ServerApp
socketApplication interpreter = do
  state <- newMVar []
  return (application state interpreter)

talk :: (Text -> IO Text) -> Client -> MVar ServerState -> IO ()
talk interpreter' (user, conn) state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData conn >>= interpreter'
      readMVar state >>= broadcast (user <> ": " <> msg)

application :: MVar ServerState -> (Text -> IO Text) -> ServerApp
application state interpreter' pending = do
  connection' <- acceptRequest pending
  forkPingThread connection' 30
  -- initialMessage <- WS.receiveData connection'
  id' <- generateID <$> readMVar state
  modifyMVar_ state $ joinClient (id', connection')
  initConnection (id', connection')
  where
    initConnection client' = finally (talk interpreter' client' state) (disconnectClient client' state)
