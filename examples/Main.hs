{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Deprecated.API         (gqlApi)
import           Mythology.API          (mythologyApi)
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  post "/api" $ raw =<< (liftIO . gqlApi =<< body)
  post "/" $ raw =<< (liftIO . mythologyApi =<< body)
