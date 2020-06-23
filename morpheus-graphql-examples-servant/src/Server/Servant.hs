{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant (servantServer) where

import Data.Morpheus (Interpreter (..))
import Network.Wai.Handler.Warp
import Servant
  ( (:<|>) (..),
    Proxy (..),
    Server,
    serve,
  )
import Server.API.Simple
  ( simpleApi,
  )
import Server.ServantGQL
  ( Endpoint,
    serveEndpoint,
  )

-- Server
type API =
  Endpoint "gql"
    :<|> Endpoint "mythology"

handler :: Server API
handler =
  serveEndpoint simpleApi
    :<|> serveEndpoint simpleApi

api :: Proxy API
api = Proxy

servantServer :: IO ()
servantServer = run 3000 (serve api handler)
