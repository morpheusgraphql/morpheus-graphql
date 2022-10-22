{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Morpheus (httpPlayground, runApp)
import Server.API (app)
import Web.Scotty (ActionM, body, get, post, raw, scotty)

runScotty :: ActionM () -> IO ()
runScotty = scotty 3000 . post "/"

main :: IO ()
main =
  scotty 3000 $ do
    get "/" (raw httpPlayground)
    post "/" (raw =<< runApp app =<< body)
