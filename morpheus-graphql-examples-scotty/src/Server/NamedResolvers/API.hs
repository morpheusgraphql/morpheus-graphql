{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Server.NamedResolvers.API
  ( app,
  )
where

import Data.Morpheus (App)
import Data.Semigroup (Semigroup ((<>)))
import Server.NamedResolvers.Authors
import Server.NamedResolvers.Posts

app :: App () IO
app = authorsApp <> postsApp
