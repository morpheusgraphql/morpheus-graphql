{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Example.Schema (resolve)
import Data.Morpheus (requestFromText)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T

main :: IO ()
main = scotty 3000 $ do
  post "/api" $ do
    response <- liftIO . resolve . requestFromText . T.pack . B.unpack =<< body
    json response
