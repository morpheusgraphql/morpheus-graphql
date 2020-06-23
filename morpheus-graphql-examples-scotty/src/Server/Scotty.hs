{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Scotty
  ( scottyServer,
  )
where

-- examples
import Client.Client
  ( fetchUser,
  )
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Document (toGraphQLDocument)
import Data.Morpheus.Server
  ( httpPubApp,
    webSocketsApp,
  )
import qualified Server.Mythology.API as Mythology (api, rootResolver)
import Server.Sophisticated.API
  ( EVENT,
    api,
    gqlRoot,
  )
import qualified Server.TH.Simple as TH (api, rootResolver)
import Server.Utils
  ( httpEndpoint,
    startServer,
  )
import Web.Scotty
  ( ScottyM,
  )

scottyServer :: IO ()
scottyServer = do
  (wsApp, publish) <- webSocketsApp api
  fetchUser (httpPubApp api publish) >>= print
  startServer wsApp (httpServer publish)
  where
    httpServer :: (EVENT -> IO ()) -> ScottyM ()
    httpServer publish = do
      httpEndpoint "/" (Just $ toGraphQLDocument $ Identity gqlRoot) (httpPubApp api publish)
      httpEndpoint "/mythology" Nothing Mythology.api
      httpEndpoint "/th" Nothing TH.api
