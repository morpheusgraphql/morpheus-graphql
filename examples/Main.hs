{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Morpheus.Server   (startWebSocket)
import           Deprecated.API         (gqlApi)
import           Mythology.API          (mythologyApi)
import           Web.Scotty

startServer :: IO ()
startServer =
  scotty 3000 $ do
    post "/api" $ raw =<< (liftIO . gqlApi =<< body)
    post "/" $ raw =<< (liftIO . mythologyApi =<< body)

main :: IO ()
main = startWebSocket
