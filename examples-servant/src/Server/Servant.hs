{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant (servantServer) where

import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
  )
import qualified Data.List.NonEmpty as NE
import Data.Morpheus (Interpreter (..))
import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Data.Typeable (Typeable)
import GHC.TypeLits
import qualified Network.HTTP.Media as M
import Network.Wai.Handler.Warp
import Servant
  ( (:<|>) (..),
    (:>),
    Accept (..),
    Get,
    JSON,
    MimeRender (..),
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
  )
import Server.Mythology.API (mythologyRoot)

data HTML deriving (Typeable)

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8")
      NE.:| ["text" M.// "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type APIInterface = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

-- HELPERS
type GQLAPI (name :: Symbol) =
  name
    :> ( APIInterface :<|> Get '[HTML] ByteString
       )

serveGQL :: (GQLRequest -> IO GQLResponse) -> Server (GQLAPI name)
serveGQL gqlApp = (liftIO . gqlApp) :<|> return "<html><head><title>Title of the document</title></head><body>hello</body></html>"

-- Server
type Mythology = GQLAPI "mythology"

type API = Mythology

gqlServer :: Server Mythology
gqlServer = serveGQL (interpreter mythologyRoot)

api :: Proxy API
api = Proxy

servantServer :: IO ()
servantServer = run 3000 (serve api gqlServer)
