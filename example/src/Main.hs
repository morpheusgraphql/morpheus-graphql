{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Monad.IO.Class (liftIO)
import Web.Scotty (scotty, get, body)
import Example.Schema (resolve)
import Data.Morpheus (requestFromText)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = scotty 3000 $
  get "/api" $ do
    gqlQuery <- body
    liftIO . resolve . requestFromText . B.pack $ gqlQuery
