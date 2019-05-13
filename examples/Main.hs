{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Control.Monad.IO.Class         ( liftIO )

import           Mythology.Mythology            ( mythologyApi )
import           Mythology.Schema               ( gqlApi )
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)
