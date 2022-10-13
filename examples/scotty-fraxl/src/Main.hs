{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

-- examples

import Fraxl.API
  ( httpEndpoint,
  )
import Web.Scotty
  ( scotty,
  )

main :: IO ()
main = scotty 3000 (httpEndpoint "/")
