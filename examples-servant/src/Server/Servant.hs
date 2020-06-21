{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant (servantServer) where

import Control.Monad.Trans (liftIO)
import Data.Morpheus (Interpreter (..))
import Data.Morpheus.Types (GQLRequest, GQLResponse)
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Server.Mythology.API (mythologyRoot)

-- HELPERS
type GQLAPI (name :: Symbol) =
  name
    :> ( ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
       )

serveGQL :: (GQLRequest -> IO GQLResponse) -> Server (GQLAPI name)
serveGQL gqlApp = liftIO . gqlApp

type GetAPI (name :: Symbol) a = name :> Get '[JSON] a

-- Server
type Mythology = GQLAPI "mythology"

type Users = GetAPI "users" String

type API =
  Users
    :<|> Mythology

gqlServer :: Server Mythology
gqlServer = serveGQL (interpreter mythologyRoot)

userServer :: Server Users
userServer = return "john, David, ...."

api :: Proxy API
api = Proxy

servantServer :: IO ()
servantServer =
  run 3000 . serve api $
    userServer
      :<|> gqlServer
