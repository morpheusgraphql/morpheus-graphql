module Main
  ( main
  )
where

import           Server.Servant                 (servantServer)
import           Server.Scotty                  (scottyServer)


main :: IO ()
main = scottyServer -- or you can use: servantServer