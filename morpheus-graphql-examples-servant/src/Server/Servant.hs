{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant (servantServer) where

import Network.Wai.Handler.Warp
import Servant
  ( (:<|>) (..),
    Proxy (..),
    Server,
    serve,
  )
import Server.API.Simple
  ( rootResolver,
    simpleApi,
  )
import Server.Utils
  ( Endpoint,
    serveEndpoint,
  )

-- Server
type API =
  Endpoint "gql"
    :<|> Endpoint "mythology"

handler :: Server API
handler =
  serveEndpoint rootResolver simpleApi
    :<|> serveEndpoint rootResolver simpleApi

api :: Proxy API
api = Proxy

servantServer :: IO ()
servantServer = run 3000 (serve api handler)
