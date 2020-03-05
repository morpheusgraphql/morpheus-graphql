module Main
  ( main
  )
where

import           Server.Servant                 (servantServer)
import           Server.Scotty                  (scottyServer)


main :: IO ()
main = servantServer