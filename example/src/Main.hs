{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Example.Schema         (resolve)
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ post "/api" $ join $ json <$> (liftIO . resolve =<< body)
