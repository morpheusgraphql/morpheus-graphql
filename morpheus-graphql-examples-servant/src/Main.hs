module Main
  ( main,
  )
where

import Server.Servant (servantServer)

main :: IO ()
main = servantServer
