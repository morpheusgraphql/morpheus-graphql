{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import HaxlAPI.API
import Web.Scotty (scotty)

main :: IO ()
main = scotty 3000 (httpEndpoint "/")
