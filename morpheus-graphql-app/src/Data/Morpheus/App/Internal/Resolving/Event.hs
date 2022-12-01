{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Event
  ( EventHandler (..),
    ResponseEvent (..),
  )
where

import Data.Kind (Type)
import Data.Morpheus.Types.IO
  ( GQLResponse,
  )

class EventHandler e where
  type Channel e :: Type

data ResponseEvent event (m :: Type -> Type)
  = Publish event
  | Subscribe
      { subChannel :: Channel event,
        subRes :: event -> m GQLResponse
      }
