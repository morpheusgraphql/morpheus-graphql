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

newtype Content
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
saveResults ss =
  writeIORef (counter ss)

savePublisher :: ServerState -> (MEvent -> IO ()) -> IO ()
savePublisher ss =
  writeIORef (publisher ss)

readPublisher :: ServerState -> IO (MEvent -> IO ())
readPublisher ss =
  readIORef $ publisher ss
