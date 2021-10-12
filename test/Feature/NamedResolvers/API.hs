{-# LANGUAGE NoImplicitPrelude #-}

module Feature.NamedResolvers.API
  ( app,
  )
where

import Data.Morpheus (App)
import Feature.NamedResolvers.Deities (deitiesApp)
import Feature.NamedResolvers.Entities (entitiesApp)
import Feature.NamedResolvers.Realms (realmsApp)
import Relude

app :: App () IO
app = deitiesApp <> realmsApp <> entitiesApp
