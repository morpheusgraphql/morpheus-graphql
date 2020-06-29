{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving.Event
  ( Event (..),
    Channel (..),
  )
where

import Prelude
  ( Eq (..),
  )

-- Channel
data Channel (event :: *) where
  Channel :: a -> Channel (Event a c)

data Event e c = Event
  {channels :: [e], content :: c}

instance (Event ch con ~ event, Eq ch) => Eq (Channel event) where
  Channel x == Channel y = x == y
