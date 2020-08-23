{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Morpheus.Server
  ( compileTimeSchemaValidation,
    httpPubApp,
    webSocketsApp,
  )
import qualified Server.Mythology.API as Mythology (api, rootResolver)
import Server.Sophisticated.API
  ( EVENT,
    api,
    root,
  )
import qualified Server.TH.Simple as TH (api, rootResolver)
import Server.Utils
  ( httpEndpoint,
    startServer,
  )
import Web.Scotty
  ( ScottyM,
  )

_validateSchema :: ()
_validateSchema = $(compileTimeSchemaValidation (Identity root))

scottyServer :: IO ()
scottyServer = do
  (wsApp, publish) <- webSocketsApp api
  fetchUser (httpPubApp api publish) >>= print
  startServer wsApp (httpApp publish)
  where
    httpApp :: (EVENT -> IO ()) -> ScottyM ()
    httpApp publish = do
      httpEndpoint "/" root (httpPubApp api publish)
      httpEndpoint "/mythology" Mythology.rootResolver Mythology.api
      httpEndpoint "/th" TH.rootResolver TH.api
