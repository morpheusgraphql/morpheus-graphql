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
import Server.API.Simple (simpleApi)
import Server.ServantGQL (GQLEndpoint, serveGQLEndpoint)

-- Server
type API =
  GQLEndpoint "gql"
    :<|> GQLEndpoint "mythology"

handler :: Server API
handler =
  serveGQLEndpoint simpleApi
    :<|> serveGQLEndpoint simpleApi

api :: Proxy API
api = Proxy

servantServer :: IO ()
servantServer = run 3000 (serve api handler)
