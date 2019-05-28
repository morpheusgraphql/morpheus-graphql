{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Concurrent     (forkIO)
import           Control.Monad.IO.Class (liftIO)
import           Data.Morpheus          (packStream, streamInterpreter)
import           Data.Morpheus.Server   (socketGQL)
import           Deprecated.API         (gqlRoot)
import           Mythology.API          (mythologyApi)
import           Network.WebSockets     (runServer)
import           Web.Scotty

{-

const ws = new WebSocket('ws://localhost:9160/');
ws.send(JSON.stringify({"query":"query GetUser{user{name}}"}))
ws.send(JSON.stringify({"query":"mutation CreateUser{ createUser{name} }"}))
ws.send(JSON.stringify({"query":"subscription ShowNewUser{ newUser{name} }"}))

-}
main :: IO ()
main = do
  _ <- forkIO wsServer
  httpServer
  where
    wsServer = socketGQL (streamInterpreter gqlRoot) >>= runServer "127.0.0.1" 4000
    httpServer =
      scotty 3000 $ do
        post "/api" $ raw =<< (liftIO . packStream (streamInterpreter gqlRoot) =<< body)
        post "/" $ raw =<< (liftIO . mythologyApi =<< body)
