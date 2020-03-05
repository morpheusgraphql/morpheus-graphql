{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}

module Server.Servant  
    (servantServer)
where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Data.Morpheus.Types  (GQLRequest, GQLResponse) 
import Server.Mythology.API           ( mythologyRoot )
import Data.Morpheus                  ( Interpreter(..) )
import Control.Monad.Trans (liftIO)

type API 
    = UserApi "users" 
    :<|> GQLAPI "gql"

newtype User = User { username :: String }
  deriving (Generic, FromJSON, ToJSON)

type UserApi (name :: Symbol) = name :> Get '[JSON] User

type GQLAPI (name :: Symbol) = name :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

userServer :: Server (UserApi "users")
userServer  = return User { username = "john"}

gqlServer ::  Server (GQLAPI "gql")
gqlServer = liftIO . mythologyApi
  where 
  mythologyApi :: GQLRequest -> IO GQLResponse
  mythologyApi = interpreter mythologyRoot

api :: Proxy API
api = Proxy

servantServer :: IO ()
servantServer = run 8080 . serve api 
    $   userServer 
  :<|>  gqlServer