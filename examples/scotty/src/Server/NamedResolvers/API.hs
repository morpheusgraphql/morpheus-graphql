{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Server.NamedResolvers.API
  ( app,
  )
where

import Data.Morpheus (App)
import Server.NamedResolvers.Authors (authorsApp)
import Server.NamedResolvers.Pages (pagesApp)
import Server.NamedResolvers.Posts (postsApp)

app :: App () IO
app = authorsApp <> postsApp <> pagesApp
