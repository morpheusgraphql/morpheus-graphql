{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Morpheus.Server   (socketApplication)
import           Deprecated.API         (gqlApi)
import           Mythology.API          (mythologyApi)
import           Network.WebSockets     (runServer)
import           Web.Scotty

startWebSocket :: IO ()
startWebSocket = socketApplication >>= runServer "127.0.0.1" 9160

startServer :: IO ()
startServer =
  scotty 3000 $ do
    post "/api" $ raw =<< (liftIO . gqlApi =<< body)
    post "/" $ raw =<< (liftIO . mythologyApi =<< body)

main :: IO ()
main = startWebSocket
