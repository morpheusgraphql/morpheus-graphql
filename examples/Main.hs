{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

-- import           Control.Monad.IO.Class (liftIO)
import           Data.Morpheus.Server   (socketApplication)
import           Deprecated.API         (gqlApi)
-- import           Mythology.API          (mythologyApi)
import           Network.WebSockets     (runServer)
-- import           Web.Scotty

startWebSocket :: IO ()
startWebSocket = socketApplication gqlApi >>= runServer "127.0.0.1" 9160

--const ws = new WebSocket('ws://localhost:9160/');

--ws.send(JSON.stringify({"query":"query GetUser{user{name}}"}))
-- ws.send(JSON.stringify({"query":"mutation CreateUser{ createUser{name} }"}))
-- ws.send(JSON.stringify({"query":"subscription ShowNewUser{ newUser{name} }"}))

{-
startServer :: IO ()
startServer =
  scotty 3000 $ do
    post "/api" $ raw =<< (liftIO . gqlApi =<< body)
    post "/" $ raw =<< (liftIO . mythologyApi =<< body)
-}


main :: IO ()
main = startWebSocket
