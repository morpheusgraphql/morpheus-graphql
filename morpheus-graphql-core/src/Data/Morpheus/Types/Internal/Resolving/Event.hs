{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Resolving.Event
  ( EventHandler (..),
  )
where

class EventHandler e where
  type Channel e
  getChannels :: e -> [Channel e]
