{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Event
  ( EventHandler (..),
    ResponseEvent (..),
  )
where

import Data.Morpheus.Types.IO
  ( GQLResponse,
  )

class EventHandler e where
  type Channel e
  getChannels :: e -> [Channel e]

data ResponseEvent event (m :: * -> *)
  = Publish event
  | Subscribe
      { subChannel :: Channel event,
        subRes :: event -> m GQLResponse
      }
