{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Morpheus (App, httpPlayground, runApp)
import Domains.API (domainsApp)
import Namespaces.API ()
import Operation.API ()
import Web.Scotty

useApp :: App e ActionM -> ScottyM ()
useApp app = do
  get "/" (raw httpPlayground)
  post "/" (raw =<< runApp app =<< body)

main :: IO ()
main = scotty 3000 (useApp domainsApp)
