{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Subscriptions.Event
  ( Event (..),
    runEvents,
  )
where

import Data.Morpheus.Types.Internal.Resolving
  ( EventHandler (..),
  )
import Relude

data Event ch con = Event
  { channels :: [ch],
    content :: con
  }

instance EventHandler (Event ch con) where
  type Channel (Event ch con) = ch
  getChannels = channels

runEvents ::
  (Foldable t, Applicative f) =>
  t (event -> f b) ->
  event ->
  f ()
runEvents fs e = traverse_ (e &) fs
