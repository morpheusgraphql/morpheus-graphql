{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Utils
  ( TrailState (..),
    trail,
    expectedResponse,
    SubM,
  )
where

import Control.Monad.State.Lazy
  ( StateT,
    runStateT,
    state,
  )
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Types
  ( Input,
    Stream,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( ClientConnectionStore,
    Scope (..),
    WS,
    runStreamWS,
  )
import Data.Semigroup ((<>))
import Test.Tasty.HUnit
  ( assertFailure,
  )

data TrailState e = TrailState
  { inputs :: [ByteString],
    outputs :: [ByteString],
    store :: ClientConnectionStore e (SubM e)
  }

type SubM e = StateT (TrailState e) IO

addOutput :: ByteString -> TrailState e -> ((), TrailState e)
addOutput x (TrailState i xs st) = ((), TrailState i (xs <> [x]) st)

updateStore :: (ClientConnectionStore e (SubM e) -> ClientConnectionStore e (SubM e)) -> TrailState e -> ((), TrailState e)
updateStore up (TrailState i o st) = ((), TrailState i o (up st))

readInput :: TrailState e -> (ByteString, TrailState e)
readInput (TrailState (i : ins) o s) = (i, TrailState ins o s)
readInput (TrailState [] o s) = ("<Error>", TrailState [] o s)

wsApp ::
  (Input WS -> Stream WS e (SubM e)) ->
  Input WS ->
  SubM e ()
wsApp api input =
  runStreamWS
    ScopeWS
      { update = state . updateStore,
        listener = state readInput,
        callback = state . addOutput
      }
    (api input)

trail ::
  (Input WS -> Stream WS e (SubM e)) ->
  Input WS ->
  TrailState e ->
  IO (TrailState e)
trail api input = fmap snd . runStateT (wsApp api input)

expectedResponse :: [ByteString] -> [ByteString] -> IO ()
expectedResponse expected value
  | expected == value = return ()
  | otherwise =
    assertFailure $ "expected: \n " <> show expected <> " \n but got: \n " <> show value
