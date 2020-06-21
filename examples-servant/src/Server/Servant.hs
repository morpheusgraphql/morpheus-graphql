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
import Servant.API.Raw (Raw)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Server.Mythology.API (mythologyRoot)

-- HELPERS
type GQLAPI (name :: Symbol) =
  name
    :> ( ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
           :<|> Raw
       )

serveGQL :: (GQLRequest -> IO GQLResponse) -> Server (GQLAPI name)
serveGQL gqlApp = (liftIO . gqlApp) :<|> serveDirectoryFileServer "examples-servant/assets/index.html"

-- Server
type Mythology = GQLAPI "mythology"

type API = Mythology

gqlServer :: Server Mythology
gqlServer = serveGQL (interpreter mythologyRoot)

api :: Proxy API
api = Proxy

servantServer :: IO ()
servantServer = run 3000 (serve api gqlServer)
