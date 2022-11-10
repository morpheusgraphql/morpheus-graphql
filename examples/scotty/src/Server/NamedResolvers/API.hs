{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server.NamedResolvers.API
  ( app,
  )
where

import Data.Morpheus (App)
import Data.Semigroup (Semigroup ((<>)))
import Server.NamedResolvers.Authors (authorsApp)
import Server.NamedResolvers.Pages (pagesApp)
import Server.NamedResolvers.Posts (postsApp)
import Prelude (IO)

app :: App () IO
app = authorsApp <> postsApp <> pagesApp
