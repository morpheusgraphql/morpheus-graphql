{-# LANGUAGE NoImplicitPrelude #-}

module Feature.NamedResolvers.API
  ( app,
  )
where

import Data.Morpheus (App)
import Feature.NamedResolvers.Deities (authorsApp)
import Feature.NamedResolvers.Entities (pagesApp)
import Feature.NamedResolvers.Realms (postsApp)
import Relude

app :: App () IO
app = authorsApp <> postsApp <> pagesApp
