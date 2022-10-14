{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Data (Typeable)
import Data.Morpheus (App, httpPlayground, runApp)
import Data.Semigroup ((<>))
import qualified Domains.Posts.Resolver as P
import qualified Domains.Users.Resolver as U
import qualified Namespaces.Mutation as NM
import qualified Namespaces.Query as NQ
import qualified Namespaces.Subscription as NS
import qualified Operation.Mutation as M
import qualified Operation.Query as Q
import qualified Operation.Subscription as S
import Web.Scotty
import Prelude (IO, Monad, ($), (.), (=<<))

mainApp :: (Typeable m, Monad m) => App () m
mainApp = P.app <> U.app

runScotty :: ActionM () -> IO ()
runScotty = scotty 3000 . post "/"

main :: IO ()
main =
  scotty 3000 $ do
    get "/" (raw httpPlayground)
    post "/" (raw =<< runApp mainApp =<< body)
