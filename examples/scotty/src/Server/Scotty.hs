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

import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Server
  ( compileTimeSchemaValidation,
  )
import Data.Morpheus.Subscriptions (webSocketsApp)
import qualified Server.Fraxl.API as Fraxl
import qualified Server.Haxl.API as Haxl
import qualified Server.Mythology.API as Mythology
import qualified Server.NamedResolvers.API as Named
import Server.Sophisticated.API
  ( EVENT,
    app,
    root,
  )
import qualified Server.TH.Simple as TH
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
  (wsApp, publish) <- webSocketsApp app
  startServer wsApp (httpApp publish)
  where
    httpApp :: (EVENT -> IO ()) -> ScottyM ()
    httpApp publish = do
      httpEndpoint "/" [publish] app
      httpEndpoint "/mythology" [] Mythology.app
      Fraxl.httpEndpoint "/fraxl"
      httpEndpoint "/th" [] TH.app
      Haxl.httpEndpoint "/haxl" Haxl.app
      httpEndpoint "/named-resolvers" [] Named.app
