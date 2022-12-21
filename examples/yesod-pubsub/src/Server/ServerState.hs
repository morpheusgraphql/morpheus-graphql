{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Server.ServerState
  ( ServerState (..),
    Channel (..),
    Content (..),
    MEvent,
    new,
    readResults,
    readPublisher,
    saveResults,
    savePublisher,
  )
where

import Control.Monad.IO.Unlift
import Data.Hashable (Hashable)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Morpheus.Subscriptions (Event (..), PubApp)
import GHC.Generics

data Channel
  = ChannelCounter
  deriving (Show, Eq, Generic, Hashable)

data Content
  = Count Int
  deriving (Show)

type MEvent = Event Channel Content

data ServerState = ServerState
  { publisher :: IORef (MEvent -> IO ()),
    counter :: IORef Int
  }

new :: IO ServerState
new = do
  initCounter <- newIORef 0
  initPublisher <- newIORef $ \_ -> return () -- Initially, NOP
  return $
    ServerState
      { counter = initCounter,
        publisher = initPublisher
      }

readResults :: ServerState -> IO Int
readResults ss =
  readIORef $ counter ss

saveResults :: ServerState -> Int -> IO ()
saveResults ss i =
  -- atomicModifyIORef (counter serverState) $ \r ->
  writeIORef (counter ss) i

savePublisher :: ServerState -> (MEvent -> IO ()) -> IO ()
savePublisher ss pub =
  writeIORef (publisher ss) pub

readPublisher :: ServerState -> IO (MEvent -> IO ())
readPublisher ss =
  readIORef $ publisher ss
