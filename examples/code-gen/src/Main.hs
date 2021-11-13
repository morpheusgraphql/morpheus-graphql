{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified API.Posts.Resolver as P
import qualified API.Users.Resolver as U
import Data.Data (Typeable)
import Data.Morpheus (App, runApp)
import Data.Morpheus.Server (httpPlayground)
import Data.Semigroup ((<>))
import Web.Scotty
import Prelude (($), (.), (=<<), IO, Monad)

mainApp :: (Typeable m, Monad m) => App () m
mainApp = P.app <> U.app

runScotty :: ActionM () -> IO ()
runScotty = scotty 3000 . post "/"

main :: IO ()
main =
  scotty 3000 $ do
    get "/" (raw httpPlayground)
    post "/" (raw =<< runApp mainApp =<< body)
