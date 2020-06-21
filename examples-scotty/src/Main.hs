module Main
  ( main,
  )
where

import Server.Scotty (scottyServer)
import Server.Servant (servantServer)

main :: IO ()
main = scottyServer

-- or
-- main = servantServer
