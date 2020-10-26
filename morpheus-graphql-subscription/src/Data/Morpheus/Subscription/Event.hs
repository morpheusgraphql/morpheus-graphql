{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Subscription.Event
  ( Event (..),
  )
where

import Data.Morpheus.Types.Internal.Resolving
  ( EventHandler (..),
  )

data Event ch con = Event
  { channels :: [ch],
    content :: con
  }

instance EventHandler (Event ch con) where
  type Channel (Event ch con) = ch
  getChannels = channels
