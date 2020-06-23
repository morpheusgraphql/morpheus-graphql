{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.ServantGQL
  ( GQLEndpoint,
    serveGQLEndpoint,
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
import Servant
  ( (:<|>) (..),
    (:>),
    Accept (..),
    Get,
    JSON,
    MimeRender (..),
    Post,
    ReqBody,
    Server,
  )

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type GQLAPI = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

type GQLEndpoint (name :: Symbol) = name :> (GQLAPI :<|> Get '[HTML] ByteString)

serveGQLEndpoint :: (GQLRequest -> IO GQLResponse) -> Server (GQLEndpoint name)
serveGQLEndpoint app = (liftIO . app) :<|> gqlPlayground

gqlPlayground :: (Monad m, MonadIO m) => m ByteString
gqlPlayground = pure playground

playground :: ByteString
playground =
  "<!DOCTYPE html> \
  \  <html> \
  \    <head> \
  \      <meta charset=\"utf-8\" /> \
  \      <meta name=\"viewport\" content=\"user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, minimal-ui\" /> \
  \      <title>GraphQL Playground</title> \
  \      <link \
  \        rel=\"stylesheet\" \
  \        href=\"//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/css/index.css\" \
  \      /> \
  \      <link \
  \        rel=\"shortcut icon\" \
  \        href=\"//cdn.jsdelivr.net/npm/graphql-playground-react/build/favicon.png\" \
  \      /> \
  \      <script  \
  \         src=\"//cdn.jsdelivr.net/npm/graphql-playground-react/build/static/js/middleware.js\"> \
  \      </script> \
  \    </head> \
  \    <body> \
  \      <div id='root'></div> \
  \      <script> \
  \        window.addEventListener('load', (_) => \
  \          GraphQLPlayground.init(document.getElementById('root'), {}) \
  \        ); \
  \      </script> \
  \    </body> \
  \  </html>"
