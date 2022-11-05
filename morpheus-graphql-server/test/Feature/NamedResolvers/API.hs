{-# LANGUAGE NoImplicitPrelude #-}

module Feature.NamedResolvers.API
  ( api,
  )
where

import Data.Morpheus.Server (App, runApp)
import Data.Morpheus.Server.Types (GQLRequest, GQLResponse)
import Feature.NamedResolvers.DeitiesApp (deitiesApp)
import Feature.NamedResolvers.EntitiesApp (entitiesApp)
import Feature.NamedResolvers.RealmsApp (realmsApp)
import Relude

app :: App () IO
app = deitiesApp <> realmsApp <> entitiesApp

api :: GQLRequest -> IO GQLResponse
api = runApp app
