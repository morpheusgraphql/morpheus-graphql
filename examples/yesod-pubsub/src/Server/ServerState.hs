{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
module Server.ServerState
  ( ServerState(..)
  , Channel(..)
  , Content(..)
  , MEvent
  , new
  , readResults
  , readPublisher
  , saveResults
  , savePublisher
  ) where

import           GHC.Generics
import           Data.Hashable (Hashable)
import           Control.Monad.IO.Unlift
import           Data.IORef (IORef, newIORef, atomicModifyIORef, writeIORef, readIORef)
import           Data.Morpheus.Subscriptions (PubApp, Event(..))

data Channel
  = ChannelCounter
  deriving (Show,Eq,Generic,Hashable)

data Content
  = Count Int
  deriving (Show)

type MEvent = Event Channel Content

data ServerState =
  ServerState
    { publisher :: IORef (MEvent -> IO ())
    , counter :: IORef Int
    }

new :: IO ServerState
new = do
  initCounter <- newIORef 0
  initPublisher <- newIORef $ \_ -> return () -- Initially, NOP
  return $ ServerState
    { counter = initCounter
    , publisher = initPublisher
    }

readResults :: ServerState -> IO Int
readResults ss =
  readIORef $ counter ss

saveResults :: ServerState -> Int -> IO ()
saveResults ss i =
  -- atomicModifyIORef (counter serverState) $ \r ->
  writeIORef (counter ss) i

savePublisher :: ServerState -> (MEvent -> IO()) -> IO()
savePublisher ss pub =
  writeIORef (publisher ss) pub

readPublisher :: ServerState -> IO (MEvent -> IO())
readPublisher ss =
  readIORef $ publisher ss
