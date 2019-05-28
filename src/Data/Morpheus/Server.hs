{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Server
  ( socketApplication
  ) where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Morpheus      (InputAction (..), OutputAction (..))
import           Data.Text          (Text, pack)
import           Network.WebSockets (Connection, ServerApp, acceptRequest, forkPingThread, receiveData, sendTextData)

type Client = (Text, Connection)

type ServerState = [Client]

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

talk :: (InputAction Text -> IO (OutputAction Text)) -> Client -> MVar ServerState -> IO ()
talk interpreter' (user, conn) state = forever handleRequest
  where
    handleRequest = do
      msg <- receiveData conn >>= \x -> interpreter' (SocketConnection 1 x)
      case msg of
        EffectPublish _ value -> readMVar state >>= broadcast (user <> ": " <> value)
        _                     -> readMVar state

registerSubscription :: MVar ServerState -> Connection -> IO Client
registerSubscription varState' connection' = do
  client' <- newClient
  modifyMVar_ varState' (addClient client')
  return client'
  where
    newClient = do
      id' <- generateID <$> readMVar varState'
      return (id', connection')
    addClient client' state' = return (client' : state')

type GQLApi = InputAction Text -> IO (OutputAction Text)

application :: GQLApi -> MVar ServerState -> ServerApp
application interpreter' state pending = do
  connection' <- acceptRequest pending
  forkPingThread connection' 30
  client' <- registerSubscription state connection'
  finally (talk interpreter' client' state) (disconnectClient client' state)

socketApplication :: GQLApi -> IO ServerApp
socketApplication interpreter = application interpreter <$> newMVar []
