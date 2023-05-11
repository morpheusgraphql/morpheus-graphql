module Lib
  ( main,
  )
where

import qualified Server.Server as WebServer

main :: IO ()
main =
  WebServer.main
