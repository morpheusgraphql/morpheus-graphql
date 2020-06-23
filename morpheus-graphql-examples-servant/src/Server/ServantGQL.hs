{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.ServantGQL
  ( Endpoint,
    serveEndpoint,
  )
where

import Control.Monad.Trans (liftIO)
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
import Server.Playground (playground)

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type API = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

type Endpoint (name :: Symbol) = name :> (API :<|> Get '[HTML] ByteString)

serveEndpoint :: (GQLRequest -> IO GQLResponse) -> Server (Endpoint name)
serveEndpoint app = (liftIO . app) :<|> playground
