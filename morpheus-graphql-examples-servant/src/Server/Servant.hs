{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant (servantServer) where

import Data.Morpheus.Subscription
  ( webSocketsApp,
  )
import Servant
  ( (:<|>) (..),
    Proxy (..),
    Server,
  )
import Server.API.Simple
  ( EVENT,
    app,
  )
import Server.Utils
  ( Endpoint,
    serveEndpoint,
    startServer,
  )

-- Server
type API =
  Endpoint "gql"
    :<|> Endpoint "mythology"

proxyApi :: Proxy API
proxyApi = Proxy

handler :: (EVENT -> IO ()) -> Server API
handler publish =
  serveEndpoint [publish] app
    :<|> serveEndpoint [] app

servantServer :: IO ()
servantServer = do
  (wsApp, publish) <- webSocketsApp app
  startServer wsApp proxyApi (handler publish)
