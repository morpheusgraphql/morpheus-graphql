{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving.Event
  ( Event (..),
    Channel (..),
    eventChannels,
  )
where

import Prelude
  ( Eq (..),
    fmap,
  )

-- Channel
data Channel (event :: *) where
  Channel :: a -> Channel (Event a c)

data Event e c = Event
  {channels :: [e], content :: c}

eventChannels :: Event e c -> [Channel (Event e c)]
eventChannels Event {channels} = fmap Channel channels

instance (Event ch con ~ event, Eq ch) => Eq (Channel event) where
  Channel x == Channel y = x == y
